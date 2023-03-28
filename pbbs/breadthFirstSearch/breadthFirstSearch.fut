import "lib/github.com/diku-dk/segmented/segmented"
import "lib/github.com/diku-dk/sorts/radix_sort"

type queuePair = {vertex: i32, parent: i32}

def get_first (q1: queuePair) (q2: queuePair): queuePair =
    if (q1.vertex != q2.vertex)
    then q1
    else {vertex = -1, parent = -1}

def remove_duplicates_GPU (queue: []queuePair) = 
    -- We need to append an extra in case the queue only contains items with the same vertex value
    -- Array copy is hopefully optimized away by the compiler
    let sorted = radix_sort_int_by_key (\q -> q.vertex) i32.num_bits i32.get_bit queue ++ [{vertex = -1, parent = -1}]
    in  map2 get_first sorted (rotate (1) sorted)

-- Unused
-- Might also not be secure as the function "(\_ b -> b)" isn't commutative
def remove_duplicates_balanced (queue: []queuePair) (nVerts: i64): [nVerts]queuePair =
    let verts = map (\q -> i64.i32 q.vertex) queue
    in hist (\_ b -> b) {vertex = -1, parent = -1} nVerts verts queue

-- Unused
-- Writes to the same array position with different values. It is, however, very fast
def remove_duplicates_insecure (queue: []queuePair) (nVerts: i64): [nVerts]queuePair =
    let tmp = replicate nVerts {vertex = -1, parent = -1}
    let verts = map (\q -> i64.i32 q.vertex) queue
    in scatter tmp verts queue

def update_parents [nVerts] (parents: *[nVerts]i32) (queue: []queuePair): *[nVerts]i32 =
    -- Set the parent of each vertex in the queue
    let qVerts = map (\q -> i64.i32 q.vertex) queue
    let qParents = map (\q -> q.parent) queue
    in scatter parents qVerts qParents

def edges_of_vertex (verts: []i32) (nEdges: i32) (edge: queuePair): i64 =
    let extended = verts ++ [nEdges]
    in i64.i32 (extended[edge.vertex + 1] - extended[edge.vertex])

def get_ith_edge_from_vert (verts: []i32) (edges: []i32) (parents: []i32) (q: queuePair) (i: i64) : queuePair =
    -- Get the i'th vertex of the edge
    let currentVert = edges[i64.i32 verts[q.vertex] + i]
    -- If it's an unseen vertex, add it to the queue with the current vertex being the parent
    -- Else return a placeholder that we filter later
    in if (parents[currentVert] == -1)
    then {vertex = currentVert, parent = q.vertex}
    else {vertex = -1, parent = -1}

def BFS [nVerts] [nEdges] (verts: [nVerts]i32) (edges: [nEdges]i32) (parents: *[nVerts]i32) (queue: *[]queuePair): [nVerts]i32 =
    -- Loop until we get an empty queue
    let (parents, _) = loop (parents, queue) while length queue > 0 do
        -- Setup a function that takes a queuePair, and returns how many vertexes goes out of it
        let get_edges_of_vert_fun = edges_of_vertex verts (i32.i64 nEdges)
        -- Setup a function that takes a queuePair and an int i, and returns the vertex pointed to by the i'th edge
        let get_ith_edge_from_vert_fun = get_ith_edge_from_vert verts edges parents

        -- Get the vertexes in the next layer
        let newQueue = expand (get_edges_of_vert_fun) (get_ith_edge_from_vert_fun) queue
        -- Remove empty spots/placeholders ({-1, -1} queuePairs)
        let filteredQueue = filter (\q -> q.parent != -1) newQueue
        -- Remove duplicates from the queue
        let noDupesQueue = remove_duplicates_GPU filteredQueue
        -- Remove empty spots/placeholders again
        let queue = filter (\q -> q.parent != -1) noDupesQueue

        in (update_parents parents queue, queue)
    in parents

def main [nVerts] [nEdges] (vertexes_enc: [nVerts]i32) (edges_enc: [nEdges]i32) =
    let start = 0

    let parents = replicate nVerts (-1) :> [nVerts]i32
    let queue = [{vertex = start, parent = start}]
    let parents = update_parents parents queue

    in BFS vertexes_enc edges_enc parents queue

-- ==
-- input @ data/randLocalGraph_J_10_20000000.in
-- output @ data/randLocalGraph_J_10_20000000.out
-- input @ data/rMatGraph_J_12_16000000.in
-- output @ data/rMatGraph_J_12_16000000.out
-- input @ data/3Dgrid_J_64000000.in
-- output @ data/3Dgrid_J_64000000.out