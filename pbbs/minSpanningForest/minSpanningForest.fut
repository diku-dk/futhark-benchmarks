import "lib/github.com/diku-dk/sorts/merge_sort"

def is_root (UFparents: []i32) (vert: i64): bool =
    UFparents[vert] < 0

def is_root32 (UFparents: []i32) (vert: i32): bool =
    UFparents[vert] < 0

def find (UFparents: []i32) (vert: i32): i32 =
    if (is_root32 UFparents vert)
        then vert
    else 
        loop p = UFparents[vert] while !(is_root32 UFparents p) do
            UFparents[p]

def findPair (UFparents: []i32) (edge: [2]i32) : ([2]i32) =
    [(find UFparents edge[0]), (find UFparents edge[1])]

-- Do 1 step of flattening, making find faster
def update_once [pLength] (UFparents: [pLength]i32) : (*[pLength]i32) =
    let indexes = iota pLength
    in map (\v -> if (is_root UFparents v) || (is_root32 UFparents UFparents[v]) then UFparents[v] else UFparents[UFparents[v]]) indexes

def ltm (a: (f64, [2]i32, i64)) (b: (f64, [2]i32, i64)): bool =
    (a.0 < b.0) || (a.0 == b.0 && (a.2 < b.2))

def link (UFparents: *[]i32) (us: []i32) (vs: []i32) : (*[]i32) =
    scatter UFparents (us |> map i64.i32) vs

def getSmallestPairs [arraySize] (edges: [arraySize][2]i32) (edge2Ids: [arraySize][2]i32) (nVerts: i64) (nEdges: i64) =
    -- The length of the flattened arrays
    let arraySizeFlat = arraySize * 2

    let flatE = flatten edges :> [arraySizeFlat]i32
    let flatE2i = flatten edge2Ids :> [arraySizeFlat]i32

    let zippedArray = zip flatE flatE2i

    let verts = map i64.i32 flatE 

    let H = hist i32.min (i32.i64 nEdges) nVerts verts flatE2i
    in filter (\i -> H[i.0] == i.1) zippedArray
        |> unzip

def getMSFEdges (smallestEdgeId: []i32) (e: [2]i32) (i: [2]i32) : ((i32, i32), i32) =
    if (smallestEdgeId[e[1]] == i[1]) then
        ((e[1], e[0]), i[1])
    else if (smallestEdgeId[e[0]] == i[0]) then
        ((e[0], e[1]), i[0])
    else
        ((-1, -1), -1)

def update[arraySize] (UFparents: *[]i32) (edges: [arraySize][2]i32) (edge2Ids: [arraySize][2]i32)
                      (smallestEdgeId: []i32) (includedEdges: *[]bool) =
    let (UVs, IDS) = unzip (map2 (getMSFEdges smallestEdgeId) edges edge2Ids)
    let (us, vs) = unzip UVs

    let UFparents = link UFparents us vs |> update_once
    let includedEdges = scatter includedEdges (map i64.i32 IDS) (replicate arraySize true)
    in (includedEdges, UFparents)

def MSF [nVerts] [nEdges] (UFparents: *[]i32) (edges: [][2]i32) (edgeIds: []i64) (edge2Ids: [][2]i32)
                          (smallestEdgeId: *[nVerts]i32) (includedEdges: *[nEdges]bool) =
    let (_, _, _, _, includedEdges) = loop (UFparents, edges, edge2Ids, smallestEdgeId, includedEdges) while (length edges > 0) do
        let (smallestTargets, smallestValues) = getSmallestPairs edges edge2Ids nVerts nEdges
        let smallestEdgeId = scatter smallestEdgeId (map (i64.i32) smallestTargets) smallestValues

        let (includedEdges, UFparents) = update UFparents edges edge2Ids smallestEdgeId includedEdges

        let (edge2Ids, edges) = map (findPair UFparents) edges |> zip edge2Ids |> filter (\e -> e.1[0] != e.1[1]) |> unzip
        in (UFparents, edges, edge2Ids, smallestEdgeId, includedEdges)
    in filter (.1) (zip edgeIds includedEdges) |> map (.0)

def main [nEdges] (edges: [nEdges][2]i32) (weights: [nEdges]f64) =
    let nVerts = flatten edges |> i32.maximum |> (+1) |> i64.i32

    let edgeIndexes = iota nEdges
    let edge2Ids = map (\i -> [i32.i64 i, i32.i64 i]) edgeIndexes :> [nEdges][2]i32

    let (_, edges, edgeIndexes) = zip3 weights edges edgeIndexes |> merge_sort ltm |> unzip3
    
    let smallestEdgeId = replicate nVerts i32.highest
    let included = replicate nEdges false

    let UFparents = replicate nEdges (-1)
    in MSF UFparents edges edgeIndexes edge2Ids smallestEdgeId included
