import "lib/github.com/diku-dk/sorts/merge_sort"

def is_root (UFparents: []i32) (vert: i64): bool =
    UFparents[vert] < 0

def is_root32 (UFparents: []i32) (vert: i32): bool =
    UFparents[vert] < 0

def find (UFparents: []i32) (vert: i32): i32 =
    if (is_root32 UFparents vert)
        then vert
    else 
        -- Due to agressive flattening, this should never actually have to loop
        loop p = UFparents[vert] while !(is_root32 UFparents p) do
            UFparents[p]

def find2 (UFparents: []i32) (edge: [2]i32) : ([2]i32) =
    [(find UFparents edge[0]), (find UFparents edge[1])]

-- Do 1 step of flattening
def update_once [pLength] (UFparents: [pLength]i32) : (*[pLength]i32) =
    let indexes = iota pLength
    -- If neither the index or its parent are roots, set the parent as the parents parent.
    in map (\v -> if (is_root UFparents v) || (is_root32 UFparents UFparents[v]) then UFparents[v] else UFparents[UFparents[v]]) indexes

-- This is safe as each element of us is unique
def link (UFparents: *[]i32) (us: []i32) (vs: []i32) =
    scatter UFparents (map i64.i32 us) vs

-- Order by weight, and if equal, by index
def ltm (a: (f64, [2]i32, i64)) (b: (f64, [2]i32, i64)): bool =
    (a.0 < b.0) || (a.0 == b.0 && (a.2 < b.2))

-- For each vertex, get the edge with the lowest index
def getLowestIndexes [arraySize] (edges: [arraySize][2]i32) (edgeIds: [arraySize]i64) (nVerts: i64) (nEdges: i64) =
    -- The length of the flattened arrays
    let arraySizeFlat = arraySize * 2

    let flatE = flatten edges |> map i64.i32 :> [arraySizeFlat]i64
    let flatE2i = map (\i -> [i, i]) edgeIds |> flatten :> [arraySizeFlat]i64

    let zippedArray = zip flatE flatE2i

    let H = hist i64.min nEdges nVerts flatE flatE2i
    in filter (\i -> H[i.0] == i.1) zippedArray
        |> unzip

-- If the edgis the one with the smallest index, return them for linking. Else return placeholder
def getMSFEdges (smallestEdgeId: []i64) (e: [2]i32) (i: i64) : ((i32, i32), i64) =
    if (smallestEdgeId[e[1]] == i) then
        ((e[1], e[0]), i)
    else if (smallestEdgeId[e[0]] == i) then
        ((e[0], e[1]), i)
    else
        ((-1, -1), -1)

-- Update the UF array and the array of included edges
def update[arraySize] (UFparents: *[]i32) (edges: [arraySize][2]i32) (edgeIds: [arraySize]i64)
                      (smallestEdgeId: []i64) (includedEdges: *[]bool) =
    let (UVs, IDS) = unzip (map2 (getMSFEdges smallestEdgeId) edges edgeIds)
    let (us, vs) = unzip UVs

    let UFparents = link UFparents us vs |> update_once
    let includedEdges = scatter includedEdges IDS (replicate arraySize true)
    in (includedEdges, UFparents)

def MSF [nVerts] [nEdges] (UFparents: *[]i32) (edges: [][2]i32) (edgeIds: *[]i64) (edgeIndexes: []i64)
                          (smallestEdgeId: *[nVerts]i64) (includedEdges: *[nEdges]bool) =
    let (_, _, _, _, includedEdges) = loop (UFparents, edges, edgeIds, smallestEdgeId, includedEdges) while (length edges > 0) do
        let (edgeIds, edges) = map (find2 UFparents) edges |> zip edgeIds |> filter (\e -> e.1[0] != e.1[1]) |> unzip

        let (smallestTargets, smallestValues) = getLowestIndexes edges edgeIds nVerts nEdges
        let smallestEdgeId = scatter smallestEdgeId smallestTargets smallestValues

        let (includedEdges, UFparents) = update UFparents edges edgeIds smallestEdgeId includedEdges
        in (UFparents, edges, edgeIds, smallestEdgeId, includedEdges)
    in filter (.1) (zip edgeIndexes includedEdges) |> map (.0)

def main [nEdges] (edges: [nEdges][2]i32) (weights: [nEdges]f64) =
    let nVerts = flatten edges |> i32.maximum |> (+1) |> i64.i32

    -- The innitial index of each vertex
    let edgeIndexes = iota nEdges
    -- The id of each vertex (Its index after sorting)
    let edgeIds = iota nEdges
    
    let (_, edges, edgeIndexes) = zip3 weights edges edgeIndexes |> merge_sort ltm |> unzip3
    
    let smallestEdgeId = replicate nVerts i64.highest
    let included = replicate nEdges false

    let UFparents = replicate nEdges (-1)
    in MSF UFparents edges edgeIds edgeIndexes smallestEdgeId included
