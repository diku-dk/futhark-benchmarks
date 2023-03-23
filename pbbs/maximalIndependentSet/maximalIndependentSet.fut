import "lib/github.com/diku-dk/cpprandom/random"
import "lib/github.com/diku-dk/cpprandom/shuffle"
import "lib/github.com/diku-dk/segmented/segmented"

module shuffle = mk_shuffle minstd_rand

def valid_neighbour (random_state: []i64) (C: []i64) (state: i64) (neighbour: i32): i32 =
    if (C[neighbour] == 1) && (random_state[neighbour] < state) then
        1
    else
        0

def edges_of_vertex (verts: []i32) (nEdges: i64) (vert: i64): i64 =
    let extended = verts ++ [(i32.i64 nEdges)]
    in i64.i32 (extended[vert + 1] - extended[vert])

def edges_of_vertex_or_0 (verts: []i32) (nEdges: i64) (newI: []i64) (vert: i64): i64 =
    if (newI[vert] == 0) then
        0
    else 
        edges_of_vertex verts nEdges vert

def can_add [nVerts] [nEdges] (vertexes: [nVerts]i32) (edges: [nEdges]i32) (random_state: [nVerts]i64) (C: [nVerts]i64) (index: i64): i64 =
    if (C[index] == 0) then
        0
    else
        let vEntry = (i64.i32 vertexes[index])
        let currentEdges = edges[vEntry:vEntry + (edges_of_vertex vertexes nEdges index)]

        let arr = map (valid_neighbour random_state C random_state[index]) currentEdges

        let valid = i32.sum arr
        in if (valid == 0) then
            1
        else
            0

def mark_neighbour (vertexes: []i32) (edges: []i32) (index: i64) (i: i64): i64 =
    let edgeStartIndex = vertexes[index]
    in (i64.i32 edges[(i64.i32 edgeStartIndex) + i])

def remove_neighbour_and_self [nVerts] (marked: []i64) (targets: []i64) (C: *[nVerts]i64): *[nVerts]i64 =
    -- Needed to write 0 into array. Please add scatterC, taking a constant instead of an array 🙏
    let zeros1 = map (\_ -> 0) marked
    let zeros2 = map (\_ -> 0) targets
    let C = scatter C targets zeros2
    in scatter C marked zeros1

-- Can probably be done without mapping over every vertex each loop, by keeping track of a queue-like array
let MIS [nVerts] (vertexes: [nVerts]i32) (edges: []i32) (random_state: [nVerts]i64) (C: *[nVerts]i64) (I: *[nVerts]i64) (indexes: []i64) (nEdges: i64) =
    -- Loop until every vertex is added to or excluded from the MIS
    let CI = loop (C, I) while (i64.sum C) > 0 do
        -- Get an array of flags for which vertexes can be added to MIS
        let newI = map (can_add vertexes edges random_state C) indexes
        -- Map the index of each 0-flag to -1, as to be ignored by scatter
        let targets = map2 (\i j -> j*i + (-1) * (1-i)) newI indexes
        -- Update our MIS with found values
        let I = scatter I targets newI

        -- For each newly added vertex, get its neighbours 
        let marked = expand (edges_of_vertex_or_0 vertexes nEdges newI) (mark_neighbour vertexes edges) indexes
        -- Remove the vectors neighbours and self
        let C = remove_neighbour_and_self marked targets C
        in (C, I)
    in CI.1

def main [nVerts] [nEdges] (vertexes_enc: [nVerts]i32) (edges_enc: [nEdges]i32) =
    let indexes = iota nVerts

    -- Random seed, could be anything
    let rng = minstd_rand.rng_from_seed [5, 3, 1, 8, 0, 0, 8]
    -- Shuffle the indexes, giving each vertex a unique random number
    let (_, random_state) = shuffle.shuffle rng indexes

    -- Vertexes no longer needed to be checked
    let C = replicate nVerts 1
    -- Vertexes part of the MIS
    let I = replicate nVerts 0

    in MIS vertexes_enc edges_enc random_state C I indexes nEdges

-- ==
-- input @ data/randLocalGraph_J_10_20000000.in
-- input @ data/rMatGraph_J_12_16000000.in
-- input @ data/3Dgrid_J_64000000.in