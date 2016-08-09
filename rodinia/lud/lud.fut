-- Sequential LU-decomposition.
--
-- ==
-- compiled input @ data/64.in
-- output @ data/64.out
-- notravis input @ data/256.in
-- output @ data/256.out
-- notravis input @ data/512.in
-- output @ data/512.out
-- notravis input @ data/2048.in
-- output @ data/2048.out

default(f64)

fun (*[n][n]f64, *[n][n]f64) lu_inplace(*[n][n]f64 a) =
  loop ((a,l,u) = (a,
                   copy(replicate(n,replicate(n,0.0))),
                   copy(replicate(n,replicate(n,0.0))))) =
    for k < n do
      let u[k,k] = a[k,k] in
      loop ((l,u)) = for i < n-k do
        let l[i+k,k] = a[i+k,k]/u[k,k] in
        let u[k,i+k] = a[k,i+k] in
        (l,u)
      in
      loop (a) = for i < n-k do
        loop (a) = for j < n-k do
          let a[i+k,j+k] = a[i+k,j+k] - l[i+k,k] * u[k,j+k] in
          a
        in a
      in (a,l,u)
  in
  (l,u)

-- transpose l
fun (*[n][n]f64, *[n][n]f64) lu_par(*[n][n]f64 a) =
  loop ((a,l,u) = (a,
                   copy(replicate(n,replicate(n,0.0))),
                   copy(replicate(n,replicate(n,0.0))))) =
    for k < n do
      let ukk    = a[k,k] in
      let u[k,k] = ukk    in
      let (l_k,u_k) = unzip( 
            map (fn (f64,f64) (int i) =>
                    if(i<k)
                    then ( l[k,i],     u[k,i] )
                    else ( a[i,k]/ukk, a[k,i] )
                , iota(n) )
        )
      in
      let l[k] = l_k in
      let u[k] = u_k in
      let a = 
        map( fn [n]f64 (int i) =>
                map( fn f64 (int j) =>
                        if(i<k) || (j<k)
                        then a[i,j]
                        else a[i,j] - l[k,i]*u[k,j]
                   , iota(n) )
           , iota(n) )

      in (a,l,u)
  in
  (l,u)

fun ([n][n]f64, [n][n]f64) main(*[n][n]f64 a) =
  -- lu_inplace(a)
  let (l,u) = lu_inplace(a) in ( l, u )
