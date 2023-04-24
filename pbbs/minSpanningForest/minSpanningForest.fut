import "lib/github.com/diku-dk/sorts/merge_sort"

def is_root (UFparents: []i32) (vert: i32): bool =
    UFparents[vert] < 0

def find (UFparents: []i32) (vert: i32): i32 =
    if (is_root UFparents vert)
        then vert
    else 
        loop p = UFparents[vert] while !(is_root UFparents p) do
            UFparents[p]

-- Do 1 step of flattening, making find faster
def update_once [pLength] (UFparents: *[pLength]i32): *[pLength]i32 =
    let indexes = iota pLength
    let pValues = map (\v -> UFparents[UFparents[v]]) indexes
    let indexes = map (\v -> if pValues[v] >0 then -1 else v) indexes
    in scatter UFparents indexes pValues


def ltm (a: (f64, [2]i32, i64)) (b: (f64, [2]i32, i64)): bool =
    (a.0 < b.0) || (a.0 == b.0 && (a.2 < b.2))

def main [nEdges] (edges: [nEdges][2]i32) (weights: [nEdges]f64) =
    -- We are allowed to do this in pbbs2fut instead and pass the value. Need to check how big the performance hit is
    let nVerts = reduce (\cMax eMax -> i32.max cMax eMax) 0 (map (\e -> i32.max e[0] e[1]) edges) + 1 |> i64.i32

    let edgeIndexes = iota nEdges

    let (weights, edges, indexes) = zip3 weights edges edgeIndexes |> merge_sort ltm |> unzip3
    
    let R = replicate nVerts i32.highest

    let included = replicate nEdges false
    let UFparents = replicate nEdges (-1)
    in 1