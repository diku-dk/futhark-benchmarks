-- Sequential LU-decomposition.
--
-- ==
-- compiled input @ data/64.in
-- output @ data/64.out
-- compiled input @ data/256.in
-- output @ data/256.out
-- compiled input @ data/512.in
-- output @ data/512.out

-- FIXME: futhark-test barfs on the largest data set.

-- compiled input @ data/2048.in
-- output @ data/2048.out

default(f64)

fun {*[[f64,n],n], *[[f64,n],n]} lu_inplace(*[[f64,n],n] a) =
  loop ({a,l,u} = {a,
                   copy(replicate(n,replicate(n,0.0))),
                   copy(replicate(n,replicate(n,0.0)))}) =
    for k < n do
      let u[k,k] = a[k,k] in
      loop ({l,u}) = for i < n-k do
        let l[i+k,k] = a[i+k,k]/u[k,k] in
        let u[k,i+k] = a[k,i+k] in
        {l,u}
      in
      loop (a) = for i < n-k do
        loop (a) = for j < n-k do
          let a[i+k,j+k] = a[i+k,j+k] - l[i+k,k] * u[k,j+k] in
          a
        in a
      in {a,l,u}
  in
  {l,u}

-- transpose l
fun {*[[f64,n],n], *[[f64,n],n]} lu_par(*[[f64,n],n] a) =
  loop ({a,l,u} = {a,
                   copy(replicate(n,replicate(n,0.0))),
                   copy(replicate(n,replicate(n,0.0)))}) =
    for k < n do
      let ukk    = a[k,k] in
      let u[k,k] = ukk    in
      let {l_k,u_k} = unzip( 
            map (fn {f64,f64} (int i) =>
                    if(i<k)
                    then { l[k,i],     u[k,i] }
                    else { a[i,k]/ukk, a[k,i] }
                , iota(n) )
        )
      in
      let l[k] = l_k in
      let u[k] = u_k in
      let a = 
        map( fn [f64,n] (int i) =>
                map( fn f64 (int j) =>
                        if(i<k) || (j<k)
                        then a[i,j]
                        else a[i,j] - l[k,i]*u[k,j]
                   , iota(n) )
           , iota(n) )

      in {a,l,u}
  in
  {l,u}

fun {[[f64,n],n], [[f64,n],n]} main(*[[f64,n],n] a) =
  -- lu_inplace(a)
  let {l,u} = lu_inplace(a) in { l, u }
