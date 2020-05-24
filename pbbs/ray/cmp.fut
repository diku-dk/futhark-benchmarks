let trues xs = xs |> map i32.bool |> i32.sum

let main actual expected =
  (trues (map2 (==) actual expected),
   trues (map2 (&&) (map (==(-1)) actual) (map (==(-1)) expected)),
   trues (map2 (&&) (map (!=(-1)) actual) (map (==(-1)) expected)),
   trues (map2 (&&) (map (==(-1)) actual) (map (!=(-1)) expected)))

