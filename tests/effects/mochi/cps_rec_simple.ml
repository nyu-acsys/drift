(* CPS conversion. Source Program: 

let rec f x = 
  ev x;
  if x <= 0 then 0
  else f (x-1)

let main (u: unit(*-:{v:Unit | unit}*)) = 
 f 10


Property: 

QSet   = [0]; 

delta  = fun evx (q, acc) -> (q, (evx + acc));

IniCfg = (0, 0);

assert = fun (q, acc) -> acc >= 0;

assertFinal = fun (q, acc) -> acc >= 0;


*)

let main prefu = 
  let ev = fun k0 q acc evx ->
             k0 q (evx + acc) () in 
  let ev_assert = fun k1 q0 acc0 x0 ->
                    let k17 q acc x14 =
                      let x16 = 0 in 
                      let x15 = acc >= x16 in  let x13 = () in 
                                               assert(x15);k1 q acc x13 in 
                    ev k17 q0 acc0 x0 in 
  let q1 = 0 in 
  let acc1 = 0 in 
  let f0 = fun k4 q3 acc3 f ->
             let f1 = fun k6 q5 acc5 _main ->
                        let k7 q6 acc6 res2 =
                          k6 q6 acc6 res2 in 
                        _main k7 q5 acc5 prefu in 
             let f2 = fun k8 q7 acc7 u ->
                        let x1 = 10 in 
                        let k9 q8 acc8 res3 =
                          k8 q8 acc8 res3 in 
                        f k9 q7 acc7 x1 in 
             let k5 q4 acc4 res1 =
               k4 q4 acc4 res1 in 
             f1 k5 q3 acc3 f2 in 
  let rec f k10 q9 acc9 x =
    let x4 = () in 
    let k11 q10 acc10 x3 =
      let x6 = 0 in 
      let x5 = x <= x6 in 
      let k12 q11 acc11 res4 =
        let x2 = x4 ; res4 in  k10 q11 acc11 x2 in 
      let k13 q12 acc12 res5 =
        let x9 = 0 in 
        k12 q12 acc12 x9 in 
      let k14 q13 acc13 res6 =
        let x8 = 1 in 
        let x7 = x - x8 in  let k15 q14 acc14 res7 =
                              k12 q14 acc14 res7 in 
                            f k15 q13 acc13 x7 in 
      if x5 then k13 q10 acc10 x5 else k14 q10 acc10 x5 in 
    ev_assert k11 q9 acc9 x in 
  let k3 q2 acc2 res0 =
    let k16 q acc x10 =
      let x12 = 0 in 
      let x11 = acc >= x12 in  assert(x11);x10 in 
    k16 q2 acc2 res0 in 
  f0 k3 q1 acc1 f