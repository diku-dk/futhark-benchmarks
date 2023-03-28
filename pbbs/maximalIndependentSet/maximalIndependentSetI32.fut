import "lib/github.com/diku-dk/cpprandom/random"
import "lib/github.com/diku-dk/cpprandom/shuffle"
import "lib/github.com/diku-dk/segmented/segmented"

module shuffle = mk_shuffle minstd_rand

def valid_neighbour (shuffled_array: []i32) (C: []i32) (state: i32) (neighbour: i32): i32 =
    if (C[neighbour] == 1) && (shuffled_array[neighbour] < state) then
        1
    else
        0

def edges_of_vertex (verts: []i32) (nEdges: i32) (vert: i32): i64 =
    let extended = verts ++ [nEdges]
    in i64.i32 (extended[vert + 1] - extended[vert])

def edges_of_vertex_or_0 (verts: []i32) (nEdges: i32) (newI: []i32) (vert: i32): i64 =
    if (newI[vert] == 0) then
        0
    else 
        edges_of_vertex verts nEdges vert

def can_add [nVertexes] [nEdges] (vertexes: [nVertexes]i32) (edges: [nEdges]i32) (shuffled_array: [nVertexes]i32) (C: [nVertexes]i32) (index: i32): i32 =
    if (C[index] == 0) then
        0
    else
        let vEntry = vertexes[index]
        let currentEdges = edges[i64.i32 vEntry:(i64.i32 vEntry) + (edges_of_vertex vertexes (i32.i64 nEdges) index)]

        let arr = map (valid_neighbour shuffled_array C shuffled_array[index]) currentEdges

        let valid = i32.sum arr
        in if (valid == 0) then
            1
        else
            0

def mark_neighbour (vertexes: []i32) (edges: []i32) (index: i32) (i: i64): i64 =
    let edgeStartIndex = vertexes[index]
    in i64.i32 edges[(i64.i32 edgeStartIndex) + i]

def remove_neighbour_and_self [nVertexes] (marked: []i64) (targets: []i64) (C: *[nVertexes]i32): *[nVertexes]i32 =
    -- Needed to write 0 into array. Please add scatterC, taking a constant instead of an array 🙏
    let zeros1 = map (\_ -> 0) marked
    let zeros2 = map (\_ -> 0) targets
    let C = scatter C targets zeros2
    in scatter C marked zeros1

-- Can probably be done without mapping over every vertex each loop, by keeping track of a queue-like array
let MIS [nVertexes] [nEdges] (vertexes: [nVertexes]i32) (edges: [nEdges]i32) (shuffled_array: [nVertexes]i32) (C: *[nVertexes]i32) (I: *[nVertexes]i32) (indexes: []i32) =
    -- Loop until every vertex is added to or excluded from the MIS
    let CI = loop (C, I) while (i32.sum C) > 0 do
        -- Get an array of flags for which vertexes can be added to MIS
        let newI = map (can_add vertexes edges shuffled_array C) indexes
        -- Map the index of each 0-flag to -1, as to be ignored by scatter
        let targets = map2 (\i j -> i64.i32 (j*i + (-1) * (1-i))) newI indexes
        -- Update our MIS with found values
        let I = scatter I targets newI

        -- For each newly added vertex, get its neighbours 
        let marked = expand (edges_of_vertex_or_0 vertexes (i32.i64 nEdges) newI) (mark_neighbour vertexes edges) indexes
        -- Remove the vectors neighbours and self
        let C = remove_neighbour_and_self marked targets C
        in (C, I)
    in CI.1

def main [nVertexes] [nEdges] (vertexes_enc: [nVertexes]i32) (edges_enc: [nEdges]i32) =
    let indexes = iota nVertexes |> (map i32.i64)

    -- Random seed, could be anything
    let rng = minstd_rand.rng_from_seed [5, 3, 1, 8, 0, 0, 8]
    -- Shuffle the indexes, giving each vertex a unique random number
    let (_, shuffled_array) = shuffle.shuffle rng indexes

    -- Vertexes no longer needed to be checked
    let C = replicate nVertexes 1 :> [nVertexes]i32
    -- Vertexes part of the MIS
    let I = replicate nVertexes 0 :> [nVertexes]i32

    in MIS vertexes_enc edges_enc shuffled_array C I indexes

-- ==
-- input @ data/randLocalGraph_J_10_20000000.in
-- input @ data/rMatGraph_J_12_16000000.in
-- input @ data/3Dgrid_J_64000000.in