-- Rodinia's Back-Propagation benchmark translated to Futhark.
--
-- ==
-- compiled input @ backprop-data/small.in
-- output @ backprop-data/small.out

fun real ETA()       = 0.3
fun real MOMENTUM()  = 0.3
fun bool INIT_ZERO() = False

fun real squash(real x) = 1.0 / (1.0 + exp(-x))
fun real ABS   (real x) = if x < 0.0 then 0.0 - x else x

-- Computational kernels

fun {real, [real,n]}
bpnn_output_error([real,n] target, [real,n] output) =
    let {errs, delta} = unzip (
        map ( fn {real,real} (real t, real o) =>
                let d = o * (1.0 - o) * (t - o) in
                { if d < 0.0 then 0.0-d else d, d }
            , zip(target,output) ) ) in
    let err = reduce(+, 0.0, errs)
    in  { err, delta }


fun {real, [real,nh]}
bpnn_hidden_error([real,no] delta_o, [[real,no],nh] who, [real,nh] hidden) =
    let {errs, delta_h} = unzip (
        map ( fn {real,real} (real hidden_el, [real] who_row) =>
                let prods  = zipWith( *, delta_o, who_row )       in
                let sumrow = reduce ( +, 0.0, prods )             in
                let new_el = hidden_el * (1.0-hidden_el) * sumrow in
                { ABS(new_el), new_el }
            , zip( hidden, who )
        ) ) in
    let err = reduce( +, 0.0, errs)
    in  { err, delta_h }

fun {[[real,ndelta],nly], [[real,ndelta],nly]}
bpnn_adjust_weights([real,ndelta] delta, [real,nlym1] ly, [[real,ndelta],nly] w, [[real,ndelta],nly] oldw) =
  let lyext = map( fn real (int k) =>
                        if k < 1 then 1.0 else unsafe ly[k-1]
                 , iota(nly)) in
  unzip (
  map ( fn {[real],[real]} ([real] w_row, [real] oldw_row, real lyk) =>
          unzip (
            map ( fn {real,real} (real w_el, real oldw_el, real delta_el, int j) =>
                    let new_dw = ETA()*delta_el*lyk + MOMENTUM()*oldw_el in
                    { w_el+new_dw, new_dw }
                , zip(w_row,oldw_row,delta,iota(ndelta)) )
          )
      , zip(w,oldw,lyext)
  )  )


fun [real,n2]
bpnn_layerforward([real,n1] l1, [[real,n2],n1] conn, [real,n2] conn_fstrow) =
  let connT     = transpose(conn) in
  let res_tmp   = map ( fn real ([real,n1] conn_tr_row) =>
                            let prods = zipWith(*, conn_tr_row, l1) in
                            reduce(+, 0.0, prods)
                      , connT ) in
  map ( fn real (real pr, real conn0) => squash(pr+conn0)
      , zip(res_tmp, conn_fstrow) )

--squash(rowsum)
--------------------------------------------------------/

fun { real, real
    , [[real,n_hid], n_inp1]
    , [[real,n_out],n_hidp1]
    }
bpnn_train_kernel( [real,n_in]             input_units
                 , [real,n_out]            target
                 , [[real,n_hid], n_inp1]  input_weights
                 , [[real,n_out],n_hidp1] hidden_weights
                 , [[real,n_hid], n_inp1]  input_prev_weights
                 , [[real,n_out],n_hidp1] hidden_prev_weights
) =
    let {inpweightsP_row0,inpweightsP} = split((1),  input_weights)        in
    let hidden_units = bpnn_layerforward(input_units,  inpweightsP, inpweightsP_row0[0]) in

    let {hidweightsP_row0,hidweightsP} = split((1), hidden_weights)        in
    let output_units = bpnn_layerforward(hidden_units, hidweightsP, hidweightsP_row0[0]) in

    let {out_err, output_delta} = bpnn_output_error(target, output_units)  in
    let {hid_err, hidden_delta} = bpnn_hidden_error(output_delta, hidweightsP, hidden_units)  in

    let {hidden_weights, hidden_prev_weights} =
            bpnn_adjust_weights(output_delta, hidden_units, hidden_weights, hidden_prev_weights) in
    let { input_weights,  input_prev_weights} =
            bpnn_adjust_weights(hidden_delta,  input_units,  input_weights,  input_prev_weights) in

    {out_err, hid_err, input_weights, hidden_weights}


----------------------------------------------------/

fun real sobolIndR( [int,num_bits] dirVct, int n ) =
    -- placed norm_fact here to check that hoisting does its job!
    let norm_fact = 1.0 / ( real(1 << num_bits) + 1.0 ) in
    let n_gray = (n >> 1) ^ n in
    let res = 0 in
    loop (res) =
      for i < num_bits do
        let t = 1 << i in
        if (n_gray & t) == t
            then res ^ dirVct[i]
            else res
    in real(res) * norm_fact

fun {[[real,n],m],[real,m]} bpnn_randomize_weights(int m, int n, int offset, [int] dirVct) =
    --let linw = map(sobolIndR(dirVct), map(+offset, iota(m*n)))
    --in  reshape((m,n), linw)
    -- OR with better structure:
    let mat =
      map( fn [real] (int i) =>
             map( fn real (int j) =>
                    -- (n+1) needed to create the sob num
                    -- as in the original Rodinia code.
                    let offs = i*(n+1) + offset + 1 in
                    sobolIndR( dirVct, offs + j )
                , iota(n) )
         , iota(m) ) in
    let vct =map( fn real (int i) =>
                    sobolIndR( dirVct, offset+i*(n+1) )
                , iota(m) )
    in {mat, vct}

fun [real,m] bpnn_randomize_row(int m, int offset, [int] dirVct) =
    map ( sobolIndR(dirVct), map(+offset, iota(m)) )

fun [real,m] bpnn_constant_row(int m, real val) =
    replicate(m, val)

fun [[real,n],m] bpnn_zero_weights(int m, int n) =
    map (fn [real] (int i) => replicate(n, 0.0)
        , iota(m) )
    --reshape((m,n), replicate(m*n, 0.0))

----------------------------------------------------/

fun { [real,n_in ]
    , [real,n_out]
    ,{[[real,n_hid], n_inp1], [real,n_inp1 ]}
    ,{[[real,n_out],n_hidp1], [real,n_hidp1]}
    , [[real,n_hid],n_inp1 ]
    , [[real,n_out],n_hidp1]
    }
bpnn_create(int n_in, int n_inp1, int n_hid, int n_hidp1, int n_out, int offset, [int] dirVct) =
  -- [n_out]
  let target = bpnn_constant_row(n_out, 0.1) in

  -- [[real,n_hidden],n_in]
  let {offset, {input_weights, input_weights_fstcol}} =
    if INIT_ZERO()
    then {  offset, {bpnn_zero_weights(n_inp1, n_hid), replicate(n_inp1, 0.0)} }
    else {  offset+n_inp1*n_hidp1,
            bpnn_randomize_weights(n_inp1, n_hid, offset, dirVct)  } in

  -- [[real,n_out],n_hidden]
  let {hidden_weights, hidden_weights_fstcol} =
              bpnn_randomize_weights(n_hidp1, n_out, offset, dirVct) in
  let offset = offset + n_hidp1*(n_out+1)                            in

  --[[real,n_hidden],n_in]
  let input_prev_weights = bpnn_zero_weights(n_inp1,  n_hid) in

  --[[real,n_out],n_hidden]
  let hidden_prev_weights= bpnn_zero_weights(n_hidp1, n_out) in

  --[n_in]
  let input_units = bpnn_randomize_row(n_in, offset+1, dirVct) in

  { input_units, target,
    {input_weights, input_weights_fstcol},
    {hidden_weights, hidden_weights_fstcol},
    input_prev_weights, hidden_prev_weights
  }

fun [[real],m] consColumn([[real,n],m] mat, [real,m] col) =
    let np1 = n+1 in
    map ( fn [real] ([real] matrow, real colelm) =>
                map ( fn real (int k) =>
                        if k < 1 then colelm else unsafe matrow[k-1]
                    , iota(n+1) )
        , zip(mat,col) )

fun { real, real, [[real]], [[real]] }
main(int n_in, [int,num_bits] dirVct) =
    let {n_inp1, n_hid, n_hidp1, n_out} = {n_in+1, 16, 16+1, 1} in
    let {   input_units, target,
           { input_weights,  input_weights_fstcol},
           {hidden_weights, hidden_weights_fstcol},
            input_prev_weights, hidden_prev_weights} =
        bpnn_create(n_in, n_inp1, n_hid, n_hidp1, n_out, 1, dirVct) in

    let { out_err, hid_err, input_weights, hidden_weights } =
        bpnn_train_kernel(  input_units, target,
                            input_weights,  hidden_weights,
                            input_prev_weights, hidden_prev_weights ) in

    let  input_weights' = consColumn( input_weights,  input_weights_fstcol) in
    let hidden_weights' = consColumn(hidden_weights, hidden_weights_fstcol) in

    {out_err, hid_err, input_weights', hidden_weights'}
