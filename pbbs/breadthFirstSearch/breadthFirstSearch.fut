import "lib/github.com/diku-dk/segmented/segmented"

type queuePair = {vertex: i32, parent: i32}

def get_parent (q: queuePair): i32 =
    q.parent

def get_vertex (q: queuePair): i32 =
    q.vertex

def num_vertexes (vertexes_enc: []i32) (nedges: i32) (nvertexes: i32) : []i32 =
    let lengths = copy vertexes_enc ++ [nedges]
    in loop (lengths) for i < nvertexes do
        lengths with [i] = lengths[i+1] - lengths[i]

def update_parentsQueue (parentsQueue: *[]queuePair) (queue: []queuePair) (split: i64): []queuePair =
    -- Set the parent of each vertex in the queue
    let verts = map (\q -> i64.i32 q.vertex) queue
    let parents = scatter parentsQueue verts queue
    in parents[:split] ++ queue

def remove_duplicates (queue: []queuePair) (nVertexes: i64): []queuePair =
    let tmp = replicate nVertexes {vertex = -1, parent = -1}
    let verts = map (\q -> i64.i32 q.vertex) queue
    in scatter tmp verts queue

def edges_of_vertex (lengths: []i32) (edge: queuePair): i64 =
    i64.i32 lengths[edge.vertex]

def get_ith_edge_from_vert (verts: []i32) (edges: []i32) (parentsQueue: []queuePair) (q: queuePair) (i: i64) : queuePair =
    -- Get the i'th vertex of the edge
    let currentVert = edges[i64.i32 verts[q.vertex] + i]
    -- If it's an unseen vertex, add it to the queue with the current vertex being the parent
    -- Else return a placeholder that we filter later
    in if (parentsQueue[currentVert].parent == -1)
        then {vertex = currentVert, parent = q.vertex}
        else {vertex = -1, parent = -1}

def BFS (verts: []i32) (edges: []i32) (lengths: []i32) (parentsQueue: *[]queuePair): []i32 =
    -- Split between parents and queue
    let split = length verts

    -- Loop until we get an empty queue
    -- I can't see how to do this another way as we don't know if we should run it again until after it's done and we can't do recursion 
    let parents = loop parentsQueue while length parentsQueue > split do
        -- Setup a function that takes a queuePair, and returns how many vertexes goes out of it
        let get_edges_of_vert_fun = edges_of_vertex lengths
        -- Setup a function that takes a queuePair and an int i, and returns the vertex pointed to by the i'th edge
        let get_ith_edge_from_vert_fun = get_ith_edge_from_vert verts edges parentsQueue

        let newQueue = expand (get_edges_of_vert_fun) (get_ith_edge_from_vert_fun) parentsQueue[split:]
        -- Remove duplicates
        let noDupesQueue = remove_duplicates newQueue split
        -- Remove empty spots/placeholders ({-1, -1} queuePairs)
        let filteredQueue = filter (\q -> q.vertex != -1) noDupesQueue -- Might be expensive
        in update_parentsQueue parentsQueue filteredQueue split
    in map get_parent parents

def main (vertexes_enc: []i32) (edges_enc: []i32) =
    let start = 0

    let nvertexes = length vertexes_enc
    let nedges = length edges_enc

    let parents = replicate nvertexes {vertex = -1, parent = -1}
    let queue = [{vertex = start, parent = start}]

    let lengths = num_vertexes vertexes_enc (i32.i64 nedges) (i32.i64 nvertexes)
    in BFS vertexes_enc edges_enc lengths (update_parentsQueue parents queue nvertexes)

-- ==
-- input @ data/randLocalGraph_J_10_10000.in
-- input @ data/randLocalGraph_J_10_1000000.in
-- input @ data/randLocalGraph_J_10_10000000.in