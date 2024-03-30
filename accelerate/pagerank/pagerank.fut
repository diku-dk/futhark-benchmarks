-- Implementation of the Pagerank example.  The port was carried out
-- by Dan Graakjær Kristensen, Jens Egholm Pedersen and Tobias Sukwon
-- Yoon
--
-- The Accelerate implementation performs some input preprocessing at
-- the same time it is parsed.  The cost of this is measured
-- separately in Accelerate.  Similarly, this implementation requires
-- the input to be presented in a specific form - most importantly,
-- that edges must be sorted according to their target page.  Because
-- Futhark does not directly support fine-grained timing of this kind,
-- we delegate this preprocessing to a separate entry point
-- ('preprocess_graph'), which is not used when benchmarking.  All the
-- data sets listed for this benchmark have already been preprocessed
-- appropriately.
--
-- The reason for this is that the preprocessing is fairly expensive;
-- seemingly corresponding to roughly twenty iterations of the actual
-- Pagerank algorithm.
-- ==
-- input @ data/small.in
-- output @ data/small.out
-- compiled input @ data/random_medium.in
-- output @ data/random_medium.out

type link = {from: i32, to: i32}

import "lib/github.com/diku-dk/segmented/segmented"

-- Calculate ranks from pages without any outbound edges
-- This defaults to the page contribution / number of pages
def calculate_dangling_ranks [n] (ranks: [n]f32) (sizes: [n]i32): *[]f32 =
  let zipped = zip sizes ranks
  let weights = map (\(size, rank) -> if size == 0 then rank else 0f32) zipped
  let total = f32.sum weights / f32.i64 n
  in ranks + total

-- Calculate ranks from all pages
-- A rank is counted as the contribution of a page / the outbound edges from that page
-- A contribution is defined as the rank of the page / the inbound edges
def calculate_page_ranks [n] (links: []link) (ranks: *[n]f32) (sizes: [n]i32): *[n]f32 =
  let froms = map (.from) links
  let tos = map (.to) links
  let get_rank (i: i32) = #[unsafe] if sizes[i] == 0 then 0f32
                                 else ranks[i] / f32.i32 sizes[i]
  let contributions = get_rank froms
  let page_flags = tos != rotate (-1) tos
  let scanned_contributions = segmented_scan (+) 0f32 page_flags contributions
  let (page_tos, page_contributions) =
    unzip (map3 (\to c flag -> if flag then (i64.i32 to, c) else (-1, c))
                tos scanned_contributions (rotate 1 page_flags))
  in spread n 0 page_tos page_contributions

def calculate_ranks [n] (links:[]link) (ranks_in: *[n]f32)
                        (sizes: [n]i32) (iterations:i32): *[n]f32 =
  loop ranks = ranks_in for _i < iterations do
    let ranks_pages = calculate_page_ranks links ranks sizes
    in calculate_dangling_ranks ranks_pages sizes

import "lib/github.com/diku-dk/sorts/radix_sort"

def sort_by_to = radix_sort i32.num_bits (\i (link: link) -> i32.get_bit i link.to)

def sort_by_from = radix_sort i32.num_bits (\i (link: link) -> i32.get_bit i link.from)

-- Compute the number of outbound links for each page.
def compute_sizes [m] (n: i64) (links: [m]link) =
  let links = sort_by_from links
  let froms = map (.from) links
  let flags = froms != (rotate (-1) froms)
  let sizes = segmented_scan (+) 0 flags (replicate m 1i32)
  let (sizes, ids, _) = unzip3 (filter (.2) (zip3 sizes froms (rotate 1 flags)))
  in spread n 0 (i64.i32 ids) sizes

entry preprocess_graph [m] (links_array: [m][2]i32): ([m]i32, [m]i32, []i32) =
  let links_by_to = sort_by_to (map (\l -> {from=l[0], to=l[1]}) links_array)
  let n = i32.maximum (map (\(x: link) -> 1 + x.from) links_by_to)
  in (map (.from) links_by_to,
      map (.to) links_by_to,
      compute_sizes (i64.i32 n) links_by_to)

def initial_ranks (n: i64): *[n]f32 =
  replicate n (1 / f32.i64 n)

def process_graph [m] [n] (links: [m]link) (sizes: [n]i32) (iterations: i32) =
  (calculate_ranks links (initial_ranks n) sizes iterations)

def main [m] (froms: [m]i32) (tos: [m]i32) (sizes: []i32) (iterations: i32) =
  let links = map2 (\from to -> {from, to}) froms tos
  in process_graph links sizes iterations
