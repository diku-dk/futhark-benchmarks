-- | ignore

import "vector"

module vector_2 = cat_vector vector_1 vector_1
module vector_3 = cat_vector vector_2 vector_1
module vector_5 = cat_vector vector_2 vector_3
module vector_8 = cat_vector vector_5 vector_3

-- ==
-- input { [1,2,3,4,5,6,7,8] }
-- input { [2,3,4,5,6,7,8,9] }
let main (xs: [8]i32) =
  xs
  |> vector_8.from_array
  |> vector_8.map (+1)
  |> vector_8.to_array
