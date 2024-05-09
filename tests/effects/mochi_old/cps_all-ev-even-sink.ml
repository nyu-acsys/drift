(* CPS conversion. Source Program: 

(* *** Based on COAR: **********************************************************
let rec sum x = if x <= 0 then 0 else x + sum (x - 1)
[@@@assert "typeof(sum) <: (x:{ x:int | true }) -> { ret : int | ret > x }"]
[@@@assert "typeof(sum) <: (x:{ x:int | x > 1 }) -> { ret : int | ret > x }"]
[@@@assert "typeof(sum) <: (x:{ x:int | true }) -> { ret : int | ret >= x }"]
****************************************************************************** *)

(* Properties: prop-all-ev-pos.eff *)

let rec sum x = 
  ev x;
  if x <= 0 then 0 else x + sum (x - 1)

let main (v:int(*-:{v:Int | true}*)) =
  if v < 0 then sum (0 - v) else sum v  


Property: 

(* Events are Even, sink state *)

QSet   = [0;1]; 

delta  = fun evx (q, acc) -> 
    if (evx mod 2 = 0) then (0, acc) else (1, acc);

IniCfg = (0, 0);

assert = fun (q, acc) -> q = 0; 


*)

let main prefv = 
  let ev = fun k0 q acc evx ->
             if ((evx % 2) = 0) then k0 0 acc () 
             else k0 1 acc () in 
  let ev_assert = fun k1 q0 acc0 x0 ->
                    let k20 q acc x15 =
                      let x17 = 0 in 
                      let x16 = q = x17 in  let x14 = () in 
                                            assert(x16);k1 q acc x14 in 
                    ev k20 q0 acc0 x0 in 
  let q1 = 0 in 
  let acc1 = 0 in 
  let f0 = fun k4 q3 acc3 sum ->
             let f1 = fun k6 q5 acc5 _main ->
                        let k7 q6 acc6 res2 =
                          k6 q6 acc6 res2 in 
                        _main k7 q5 acc5 prefv in 
             let f2 = fun k8 q7 acc7 v ->
                        let x2 = 0 in 
                        let x1 = v < x2 in 
                        let k9 q8 acc8 res3 =
                          k8 q8 acc8 res3 in 
                        let k10 q9 acc9 res4 =
                          let x4 = 0 in 
                          let x3 = x4 - v in  let k13 q12 acc12 res7 =
                                                k9 q12 acc12 res7 in 
                                              sum k13 q9 acc9 x3 in 
                        let k11 q10 acc10 res5 =
                          let k12 q11 acc11 res6 =
                            k9 q11 acc11 res6 in 
                          sum k12 q10 acc10 v in 
                        if x1 then k10 q7 acc7 x1 else k11 q7 acc7 x1 in 
             let k5 q4 acc4 res1 =
               k4 q4 acc4 res1 in 
             f1 k5 q3 acc3 f2 in 
  let rec sum k14 q13 acc13 x =
    let x7 = () in 
    let k15 q14 acc14 x6 =
      let x9 = 0 in 
      let x8 = x <= x9 in 
      let k16 q15 acc15 res8 =
        let x5 = x7 ; res8 in  k14 q15 acc15 x5 in 
      let k17 q16 acc16 res9 =
        let x13 = 0 in 
        k16 q16 acc16 x13 in 
      let k18 q17 acc17 res10 =
        let x12 = 1 in 
        let x11 = x - x12 in  let k19 q18 acc18 res11 =
                                let x10 = x + res11 in  k16 q18 acc18 x10 in 
                              sum k19 q17 acc17 x11 in 
      if x8 then k17 q14 acc14 x8 else k18 q14 acc14 x8 in 
    ev_assert k15 q13 acc13 x in 
  let k3 q2 acc2 res0 =
    res0 in 
  f0 k3 q1 acc1 sum