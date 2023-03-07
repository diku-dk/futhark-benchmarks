type queuePair = {vertex: i32, parent: i32}

def num_vertexes (vertexes_enc: []i32) (nedges: i32) (nvertexes: i32) : []i32 =
    let lengths = copy vertexes_enc ++ [nedges]
    in loop (lengths) for i < nvertexes do
        lengths with [i] = lengths[i+1] - lengths[i]

def update_parentsQueue (parentsQueue: *[]queuePair) (queue: []queuePair) (split: i64) =
    -- Set the parent of each vertex in the queue
    let parents = loop (parentsQueue) for q in queue do
        parentsQueue with [q.vertex] = {vertex = -1, parent = q.parent}
    in parents[:split] ++ queue

def get_parent (q: queuePair): i32 =
    q.parent

def get_vertex (q: queuePair): i32 =
    q.vertex

def get_new_edges_from_vert (q: queuePair) (verts: []i32) (edges: []i32) (lengths: []i32) (parentsQueue: []queuePair) : []queuePair =
    let edgepos = verts[q.vertex]
    -- Loop over every edge of the current vertex
    let range = 0...(lengths[q.vertex] - 1)
    in loop p2 = [] for i in range do
        -- Get the vertex the edge points to
        let currentVert = edges[edgepos + i]
        -- If it's new, add it to the queue
        let addition = if (parentsQueue[currentVert].parent == -1)
            then [{vertex = currentVert, parent = q.vertex}]
            else []
        in p2 ++ addition

def BFS (verts: []i32) (edges: []i32) (lengths: []i32) (parentsQueue: *[]queuePair): []i32 =
    -- Split between parents and queue
    let split = length verts

    -- Loop until we get an empty queue
    let parents = loop parentsQueue while length parentsQueue > split do
        -- Loop over every verteX in the queue (can be map)
        let newQueue = loop p = [] for q in parentsQueue[split:] do
            -- The edges entry point
            let newVertexes = get_new_edges_from_vert q verts edges lengths parentsQueue
            in p ++ newVertexes
        in update_parentsQueue parentsQueue newQueue split
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