-- | Distance measures.

-- | A module containing a distance measure for some numeric type.
module type distance = {
  type real
  val distance [n]: [n]real -> [n]real -> real
}

-- | A relative distance measure.
module relative_distance(R: real): distance with real = R.t = {
  type real = R.t

  let distance [num_observed] (observed: [num_observed]real) (prices: [num_observed]real): real =
    let norm (price: real) (quote: real) =
      R.(let rel = (price - quote) / quote
         in rel * rel)
    in R.sum (opaque (map2 norm observed prices))
}

-- | An absolute distance measure.
module absolute_distance(R: real): distance with real = R.t = {
  type real = R.t

  let distance [num_observed] (observed: [num_observed]real) (prices: [num_observed]real): real =
    let norm (price: real) (quote: real) =
      R.(let dif = price - quote
         in dif * dif)
    in R.sum (opaque (map2 norm observed prices))
}
