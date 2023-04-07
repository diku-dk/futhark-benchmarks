def getSmallest [arraySize] (edges: [arraySize][2]i32) (edge2Ids: [arraySize][2]i32) (nVerts: i64) (nEdges: i64) =
    let l = arraySize * 2
    let zippedArray = zip (flatten edges :> [l]i32) (flatten edge2Ids :> [l]i32)

    let verts = map ((.0) >-> i64.i32) zippedArray 
    let idIndex = zip (map (.1) zippedArray) (iota l) 
    in hist (\i1 i2 -> if i1.0 < i2.0 then i1 else i2) ((i32.i64 nEdges), -1) nVerts verts idIndex
        |> map (.1)
        |> filter (\i -> i >= 0)
        |> map (\i -> zippedArray[i])

def getMMEdges (smallestEdge: []i32) (e: [2]i32) (i: [2]i32) =
    if smallestEdge[e[0]] == i[0] && smallestEdge[e[1]] == i[0] then ((e, i): ([2]i32, [2]i32)) else ([-1, -1], [-1, -1])

def update [nVerts] [nEdges] [arraySize] (edges: [arraySize][2]i32) (edge2Ids: [arraySize][2]i32) (smallestEdge: [nVerts]i32)
                                         (markedVerts: *[nVerts]bool) (includedEdges: *[nEdges]bool) =
    let (e, ei2) = unzip (map2 (getMMEdges smallestEdge) edges edge2Ids)

    let l = arraySize*2

    let flatE = flatten e :> [l]i32
    let flatEi2 = flatten ei2 :> [l]i32

    let trues = replicate l true
    let markedVerts = scatter markedVerts (map i64.i32 (flatE)) trues
    let includedEdges = scatter includedEdges (map i64.i32 (flatEi2)) trues
    in (markedVerts, includedEdges)

def removeMarked [nEdges] [nVerts] (markedVerts: [nVerts]bool) (edges: [nEdges][2]i32) (edge2Ids: [nEdges][2]i32) = 
    zip edges edge2Ids
        |> filter (\(v, _) -> !(markedVerts[v[0]] || markedVerts[v[1]]))
        |> unzip

def resetSmallestEdge (smallestEdge: []i32) =
    map (\_ -> i32.highest) smallestEdge

def MM [nVerts] [nEdges] (edges: *[][2]i32) (edgeIds: [nEdges]i64) (edge2Ids: *[][2]i32) (markedVerts: *[nVerts]bool) (smallestEdge: *[nVerts]i32) (includedEdges: *[nEdges]bool) =
    let finished = false
    let (_, _, _, _, includedEdges, _) = loop (edges, edge2Ids, markedVerts, smallestEdge, includedEdges, finished) while (length edges > 0) do
        let smallestPairs = getSmallest edges edge2Ids nVerts nEdges

        let smallestEdge = scatter smallestEdge (map ((.0) >-> i64.i32) smallestPairs) (map (.1) smallestPairs)

        let (markedVerts, includedEdges) = update edges edge2Ids smallestEdge markedVerts includedEdges

        let (edges, edge2Ids) = removeMarked markedVerts edges edge2Ids
        
        in (edges, edge2Ids, copy markedVerts, smallestEdge, includedEdges, finished)
    in filter (.1) (zip edgeIds includedEdges) |> map (.0)

def main [nEdges] (edges_enc: *[nEdges][2]i32) =
    -- We are allowed to do this in pbbs2fut instead and pass the value. Need to check how big the performance hit is
    let nVerts = reduce (\cMax eMax -> i32.max cMax eMax) 0 (map (\e -> i32.max e[0] e[1]) edges_enc) + 1 |> i64.i32

    let edgeIds = iota nEdges
    let edge2Ids = map (\i -> [i32.i64 i, i32.i64 i]) edgeIds :> [nEdges][2]i32

    let markedVerts = replicate nVerts false
    let smallestEdge = replicate nVerts i32.highest

    let includedEdges = replicate nEdges false

    in MM edges_enc edgeIds edge2Ids markedVerts smallestEdge includedEdges

-- ==
-- input @ data/rMatGraph_E_10_10000000.in
-- output @ data/rMatGraph_E_10_10000000.out
