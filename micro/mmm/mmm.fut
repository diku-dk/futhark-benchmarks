-- ==
--
-- compiled random input {[1024][1024]f32 [1024][1024]f32} auto output
-- compiled random input {[2048][4096]f32 [4096][2048]f32} auto output
--
-- input {
--   [ [1.0f32, 2.0f32, 3.0f32], [3.0f32, 4.0f32, 5.0f32] ]
--   [ [1.0f32, 2.0f32], [3.0f32, 4.0f32], [5.0f32, 6.0f32] ]
-- }
-- output {
--   [ [22.0f32, 28.0f32], [40.0f32, 52.0f32] ]
-- }
--


def main [m][n][q]  (A: [m][q]f32) 
                    (B: [q][n]f32)
                  : [m][n]f32 =
    map(\Arow ->
            map (\Bcol ->
                    let r = map2 (\a b -> a*b) Arow Bcol |>
                            reduce (+) 0.0f32
                    in  r
                ) (transpose B)
        ) A

def main_inst (A: [1024][1024]f32) 
         (B: [1024][1024]f32)
         : [1024][1024]f32 =
    map(\Arow ->
            map (\Bcol ->
                    let r = map2 (\a b -> a*b) Arow Bcol |>
                            reduce (+) 0.0f32
                    in  r
                ) (transpose B)
        ) A
