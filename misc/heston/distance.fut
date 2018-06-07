-- | Distance measures.

-- | A module containing a distance measure for some numeric type.
module type distance = {
  type real
  val distance [n]: [n]real -> [n]real -> real
}

-- | An absolute distance measure.
module absolute_distance(R: real): distance with real = R.t = {
  type real = R.t

  open R

  let distance [num_observed] (observed: [num_observed]real) (prices: [num_observed]real): real =
    let norm (price: real) (quote: real) =
      (let rel = (price - quote) / quote
       in rel * rel)
    in R.sum (map2 norm observed prices)
}

-- | A relative distance measure.
module relative_distance(R: real): distance with real = R.t = {
  type real = R.t

  open R

  let distance [num_observed] (observed: [num_observed]real) (prices: [num_observed]real): real =
    let norm (price: real) (quote: real) =
      (let dif = price - quote
       in dif * dif)
    let a = map2 norm observed prices
    in R.sum (intrinsics.opaque a)
}
