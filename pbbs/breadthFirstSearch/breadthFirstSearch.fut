import "lib/github.com/diku-dk/segmented/segmented"

type queuePair = {vertex: i32, parent: i32}

def get_parent (q: queuePair): i32 =
    q.parent

def get_vertex (q: queuePair): i32 =
    q.vertex

def update_parentsQueue [n] (parents: *[n]i32) (queue: []queuePair): *[n]i32 =
    -- Set the parent of each vertex in the queue
    let qVerts = map (\q -> i64.i32 q.vertex) queue
    let qParents = map (\q -> q.parent) queue
    let parents = scatter parents qVerts qParents
    in parents

def remove_duplicates (queue: []queuePair) (nVerts: i64): []queuePair =
    let tmp = replicate nVerts {vertex = -1, parent = -1}
    let verts = map (\q -> i64.i32 q.vertex) queue
    in scatter tmp verts queue

def edges_of_vertex (verts: []i32) (nedges: i32) (edge: queuePair): i64 =
    let extended = verts ++ [nedges]
    in i64.i32 (extended[edge.vertex + 1] - extended[edge.vertex])

def get_ith_edge_from_vert (verts: []i32) (edges: []i32) (parents: []i32) (q: queuePair) (i: i64) : queuePair =
    -- Get the i'th vertex of the edge
    let currentVert = edges[i64.i32 verts[q.vertex] + i]
    -- If it's an unseen vertex, add it to the queue with the current vertex being the parent
    -- Else return a placeholder that we filter later
    in if (parents[currentVert] == -1)
        then {vertex = currentVert, parent = q.vertex}
        else {vertex = -1, parent = -1}

def BFS [n] (verts: []i32) (nVerts: i64) (edges: []i32) (nEdges: i64) (parents: *[n]i32) (queue: *[]queuePair): [n]i32 =
    -- Loop until we get an empty queue
    -- I can't see how to do this another way as we don't know if we should run it again until after it's done and we can't do recursion 
    let out = loop (parents, queue) while length queue > 0 do
        -- Setup a function that takes a queuePair, and returns how many vertexes goes out of it
        let get_edges_of_vert_fun = edges_of_vertex verts (i32.i64 nEdges)
        -- Setup a function that takes a queuePair and an int i, and returns the vertex pointed to by the i'th edge
        let get_ith_edge_from_vert_fun = get_ith_edge_from_vert verts edges parents

        let newQueue = expand (get_edges_of_vert_fun) (get_ith_edge_from_vert_fun) queue
        -- Remove duplicates from queue
        let noDupesQueue = remove_duplicates newQueue nVerts
        -- Remove empty spots/placeholders ({-1, -1} queuePairs)
        -- # TODO: Check if it's more efficient to filter the queue, or to add a checks everywhere that ignore vertexes with id "-1"
        let queue = filter (\q -> q.vertex != -1) noDupesQueue -- Might be expensive
        in (update_parentsQueue parents queue, queue)
    in out.0

def main (vertexes_enc: []i32) (edges_enc: []i32) =
    let start = 0

    let nVerts = length vertexes_enc
    let nEdges = length edges_enc

    let parents = replicate nVerts (-1) :> []i32
    let queue = [{vertex = start, parent = start}]
    let parents = update_parentsQueue parents queue

    in BFS vertexes_enc nVerts edges_enc nEdges parents queue

-- ==
-- input @ data/randLocalGraph_J_10_10000000.in
-- input @ data/rMatGraph_J_12_10000000.in
-- input @ data/gridGraph_J_3_10000000.in