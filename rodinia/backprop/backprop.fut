-- Rodinia's Back-Propagation benchmark translated to Futhark.
--
-- ==
-- compiled input @ data/medium.in

-- compiled input @ data/small.in
-- output @ data/small.out

default(f32)

fun f32 ETA()       = 0.3
fun f32 MOMENTUM()  = 0.3
fun bool INIT_ZERO() = False

fun f32 squash(f32 x) = 1.0 / (1.0 + exp32(-x))
fun f32 ABS   (f32 x) = if x < 0.0 then 0.0 - x else x

-- Computational kernels

fun {f32, [f32,n]}
bpnn_output_error([f32,n] target, [f32,n] output) =
    let {errs, delta} = unzip (
        map ( fn {f32,f32} (f32 t, f32 o) =>
                let d = o * (1.0 - o) * (t - o) in
                { if d < 0.0 then 0.0-d else d, d }
            , zip(target,output) ) ) in
    let err = reduce(+, 0.0, errs)
    in  { err, delta }


fun {f32, [f32,nh]}
bpnn_hidden_error([f32,no] delta_o, [[f32,no],nh] who, [f32,nh] hidden) =
    let {errs, delta_h} = unzip (
        map ( fn {f32,f32} (f32 hidden_el, [f32] who_row) =>
                let prods  = zipWith( *, delta_o, who_row )       in
                let sumrow = reduce ( +, 0.0, prods )             in
                let new_el = hidden_el * (1.0-hidden_el) * sumrow in
                { ABS(new_el), new_el }
            , zip( hidden, who )
        ) ) in
    let err = reduce( +, 0.0, errs)
    in  { err, delta_h }

fun {[[f32,ndelta],nly], [[f32,ndelta],nly]}
bpnn_adjust_weights([f32,ndelta] delta, [f32,nlym1] ly, [[f32,ndelta],nly] w, [[f32,ndelta],nly] oldw) =
  let lyext = map( fn f32 (int k) =>
                        if k < 1 then 1.0 else unsafe ly[k-1]
                 , iota(nly)) in
  unzip (
  map ( fn {[f32],[f32]} ([f32] w_row, [f32] oldw_row, f32 lyk) =>
          unzip (
            map ( fn {f32,f32} (f32 w_el, f32 oldw_el, f32 delta_el, int j) =>
                    let new_dw = ETA()*delta_el*lyk + MOMENTUM()*oldw_el in
                    { w_el+new_dw, new_dw }
                , zip(w_row,oldw_row,delta,iota(ndelta)) )
          )
      , zip(w,oldw,lyext)
  )  )


fun [f32,n2]
bpnn_layerforward([f32,n1] l1, [[f32,n2],n1] conn, [f32,n2] conn_fstrow) =
  let connT     = transpose(conn) in
  let res_tmp   = map ( fn f32 ([f32,n1] conn_tr_row) =>
                            let prods = zipWith(*, conn_tr_row, l1) in
                            reduce(+, 0.0, prods)
                      , connT ) in
  map ( fn f32 (f32 pr, f32 conn0) => squash(pr+conn0)
      , zip(res_tmp, conn_fstrow) )

--squash(rowsum)
--------------------------------------------------------/

fun { f32, f32
    , [[f32,n_hid], n_inp1]
    , [[f32,n_out],n_hidp1]
    }
bpnn_train_kernel( [f32,n_in]             input_units
                 , [f32,n_out]            target
                 , [[f32,n_hid], n_inp1]  input_weights
                 , [[f32,n_out],n_hidp1] hidden_weights
                 , [[f32,n_hid], n_inp1]  input_prev_weights
                 , [[f32,n_out],n_hidp1] hidden_prev_weights
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

fun f32 sobolIndR( [int,num_bits] dirVct, int n ) =
    -- placed norm_fact here to check that hoisting does its job!
    let norm_fact = 1.0 / ( f32(1 << num_bits) + 1.0 ) in
    let n_gray = (n >> 1) ^ n in
    let res = 0 in
    loop (res) =
      for i < num_bits do
        let t = 1 << i in
        if (n_gray & t) == t
            then res ^ dirVct[i]
            else res
    in f32(res) * norm_fact

fun {[[f32,n],m],[f32,m]} bpnn_randomize_weights(int m, int n, int offset, [int] dirVct) =
    --let linw = map(sobolIndR(dirVct), map(+offset, iota(m*n)))
    --in  reshape((m,n), linw)
    -- OR with better structure:
    let mat =
      map( fn [f32] (int i) =>
             map( fn f32 (int j) =>
                    -- (n+1) needed to create the sob num
                    -- as in the original Rodinia code.
                    let offs = i*(n+1) + offset + 1 in
                    sobolIndR( dirVct, offs + j )
                , iota(n) )
         , iota(m) ) in
    let vct =map( fn f32 (int i) =>
                    sobolIndR( dirVct, offset+i*(n+1) )
                , iota(m) )
    in {mat, vct}

fun [f32,m] bpnn_randomize_row(int m, int offset, [int] dirVct) =
    map ( sobolIndR(dirVct), map(+offset, iota(m)) )

fun [f32,m] bpnn_constant_row(int m, f32 val) =
    replicate(m, val)

fun [[f32,n],m] bpnn_zero_weights(int m, int n) =
    map (fn [f32] (int i) => replicate(n, 0.0)
        , iota(m) )
    --reshape((m,n), replicate(m*n, 0.0))

----------------------------------------------------/

fun { [f32,n_in ]
    , [f32,n_out]
    ,{[[f32,n_hid], n_inp1], [f32,n_inp1 ]}
    ,{[[f32,n_out],n_hidp1], [f32,n_hidp1]}
    , [[f32,n_hid],n_inp1 ]
    , [[f32,n_out],n_hidp1]
    }
bpnn_create(int n_in, int n_inp1, int n_hid, int n_hidp1, int n_out, int offset, [int] dirVct) =
  -- [n_out]
  let target = bpnn_constant_row(n_out, 0.1) in

  -- [[f32,n_hidden],n_in]
  let {offset, {input_weights, input_weights_fstcol}} =
    if INIT_ZERO()
    then {  offset, {bpnn_zero_weights(n_inp1, n_hid), replicate(n_inp1, 0.0)} }
    else {  offset+n_inp1*n_hidp1,
            bpnn_randomize_weights(n_inp1, n_hid, offset, dirVct)  } in

  -- [[f32,n_out],n_hidden]
  let {hidden_weights, hidden_weights_fstcol} =
              bpnn_randomize_weights(n_hidp1, n_out, offset, dirVct) in
  let offset = offset + n_hidp1*(n_out+1)                            in

  --[[f32,n_hidden],n_in]
  let input_prev_weights = bpnn_zero_weights(n_inp1,  n_hid) in

  --[[f32,n_out],n_hidden]
  let hidden_prev_weights= bpnn_zero_weights(n_hidp1, n_out) in

  --[n_in]
  let input_units = bpnn_randomize_row(n_in, offset+1, dirVct) in

  { input_units, target,
    {input_weights, input_weights_fstcol},
    {hidden_weights, hidden_weights_fstcol},
    input_prev_weights, hidden_prev_weights
  }

fun [[f32],m] consColumn([[f32,n],m] mat, [f32,m] col) =
    let np1 = n+1 in
    map ( fn [f32] ([f32] matrow, f32 colelm) =>
                map ( fn f32 (int k) =>
                        if k < 1 then colelm else unsafe matrow[k-1]
                    , iota(n+1) )
        , zip(mat,col) )

fun { f32, f32, [[f32]], [[f32]] }
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
