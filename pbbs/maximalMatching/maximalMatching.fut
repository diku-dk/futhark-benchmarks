-- Return the edge-id pairs with the smallest edge id
def getSmallestPairs [arraySize] (edges: [arraySize][2]i32) (edge2Ids: [arraySize][2]i32) (nVerts: i64) (nEdges: i64): ([]i32, []i32) =
    -- The length of the flattened arrays
    let arraySizeFlat = arraySize * 2
    
    let flatE = flatten edges :> [arraySizeFlat]i32
    let flatE2i = flatten edge2Ids :> [arraySizeFlat]i32

    let zippedArray = zip flatE flatE2i

    let verts = i64.i32 flatE

    let H = hist i32.min (i32.i64 nEdges) nVerts verts flatE2i
    in filter (\i -> H[i.0] == i.1) zippedArray
        |> unzip

-- Return the edge if it's ID is the smallest, else return placeholder
def getMMEdges (smallestEdgeId: []i32) (e: [2]i32) (i: [2]i32): ([2]i32, [2]i32) =
    if smallestEdgeId[e[0]] == i[0] && smallestEdgeId[e[1]] == i[0] then (e, i) else ([-1, -1], [-1, -1])

-- Update the marked vertexes and included edges
def update [arraySize] (edges: [arraySize][2]i32) (edge2Ids: [arraySize][2]i32) (smallestEdgeId: []i32)
                       (markedVerts: *[]bool) (includedEdges: *[]bool): (*[]bool, *[]bool) =
    -- The length of the flattened arrays
    let arraySizeFlat = arraySize*2

    let (e, e2i) = unzip (getMMEdges smallestEdgeId edges edge2Ids)
    let flatE = flatten e :> [arraySizeFlat]i32
    let flatEi2 = flatten e2i :> [arraySizeFlat]i32

    let trues = replicate arraySizeFlat true

    let markedVerts = scatter markedVerts (i64.i32 flatE) trues
    let includedEdges = scatter includedEdges (i64.i32 flatEi2) trues
    in (markedVerts, includedEdges)

-- Remove the marked edges
def removeMarked [arraySize] (markedVerts: []bool) (edges: [arraySize][2]i32) (edge2Ids: [arraySize][2]i32): ([][2]i32, [][2]i32) = 
    zip edges edge2Ids
        |> filter (\(v, _) -> !(markedVerts[v[0]] || markedVerts[v[1]]))
        |> unzip

-- Reset the smallest id of each vertex
def resetsmallestEdgeId (smallestEdgeId: []i32): *[]i32 =
    map (\_ -> i32.highest) smallestEdgeId

def MM [nVerts] [nEdges] (edges: [][2]i32) (edgeIds: [nEdges]i64) (edge2Ids: [][2]i32) (markedVerts: *[nVerts]bool)
                         (smallestEdgeId: *[nVerts]i32) (includedEdges: *[nEdges]bool) =
    let (_, _, _, _, includedEdges) = loop (edges, edge2Ids, markedVerts, smallestEdgeId, includedEdges) while (length edges > 0) do
        let (smallestTargets, smallestValues) = getSmallestPairs edges edge2Ids nVerts nEdges

        let smallestEdgeId = scatter smallestEdgeId (i64.i32 smallestTargets) smallestValues

        let (markedVerts, includedEdges) = update edges edge2Ids smallestEdgeId markedVerts includedEdges

        let (edges, edge2Ids) = removeMarked markedVerts edges edge2Ids

        let smallestEdgeId = resetsmallestEdgeId smallestEdgeId
        -- I don't get why this copy is needed. I feel like it shouldn't be
        in (edges, edge2Ids, copy markedVerts, smallestEdgeId, includedEdges)
    in filter (.1) (zip edgeIds includedEdges) |> map (.0)

def main [nEdges] (edges_enc: *[nEdges][2]i32) =
    let nVerts = flatten edges_enc |> i32.maximum |> (+1) |> i64.i32

    let edgeIds = iota nEdges
    -- Create a doubled iota to simplify scatter/map on flattened edges
    let edge2Ids = map (\i -> [i32.i64 i, i32.i64 i]) edgeIds :> [nEdges][2]i32

    let markedVerts = replicate nVerts false
    let smallestEdgeId = replicate nVerts i32.highest

    let includedEdges = replicate nEdges false

    in MM edges_enc edgeIds edge2Ids markedVerts smallestEdgeId includedEdges

-- ==
-- mem_16gb input @ data/randLocalGraph_E_10_20000000.in
-- output @ data/randLocalGraph_E_10_20000000.out
-- mem_16gb input @ data/rMatGraph_E_10_20000000.in
-- output @ data/rMatGraph_E_10_20000000.out
-- input @ data/2Dgrid_E_64000000.in
-- output @ data/2Dgrid_E_64000000.out
