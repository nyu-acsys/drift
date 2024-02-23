(* CPS conversion. Source Program: 


let sum x = 
  ev 1; (fun y -> x + y)

let main (u: unit(*-:{v:Unit | unit}*)) = 
  ev 2;
  ev (sum (ev 3; 10) (ev 4; 20))


Property: 

QSet   = [0]; 

delta  = fun evx (q, acc) -> (q, (evx + acc));

IniCfg = (0, 0);

assert = fun (q, acc) -> acc >= 0;

assertFinal = fun (q, acc) -> acc = 40;


*)

let main prefu = 
  let ev = fun k0 q acc evx ->
             k0 q (evx + acc) () in 
  let ev_assert = fun k1 q0 acc0 x0 ->
                    let k19 q acc x26 =
                      let x28 = 0 in 
                      let x27 = acc >= x28 in  let x25 = () in 
                                               assert(x27);k1 q acc x25 in 
                    ev k19 q0 acc0 x0 in 
  let q1 = 0 in 
  let acc1 = 0 in 
  let f0 = fun k4 q3 acc3 sum ->
             let f1 = fun k6 q5 acc5 _main ->
                        let k7 q6 acc6 res2 =
                          k6 q6 acc6 res2 in 
                        _main k7 q5 acc5 prefu in 
             let f2 = fun k8 q7 acc7 u ->
                        let x4 = 2 in 
                        let x3 = () in 
                        let k9 q8 acc8 x2 =
                          let x10 = 3 in 
                          let x9 = () in 
                          let k13 q12 acc12 x8 =
                            let x11 = 10 in 
                            let x7 = x9 ; x11 in 
                            let k12 q11 acc11 res4 =
                              let x15 = 4 in 
                              let x14 = () in 
                              let k14 q13 acc13 x13 =
                                let x16 = 20 in 
                                let x12 = x14 ; x16 in 
                                let k11 q10 acc10 res3 =
                                  let x6 = () in 
                                  let k10 q9 acc9 x5 =
                                    let x1 = x3 ; x6 in  k8 q9 acc9 x1 in 
                                  ev_assert k10 q10 acc10 res3 in 
                                res4 k11 q13 acc13 x12 in 
                              ev_assert k14 q11 acc11 x15 in 
                            sum k12 q12 acc12 x7 in 
                          ev_assert k13 q8 acc8 x10 in 
                        ev_assert k9 q7 acc7 x4 in 
             let k5 q4 acc4 res1 =
               k4 q4 acc4 res1 in 
             f1 k5 q3 acc3 f2 in 
  let f3 = fun k15 q14 acc14 x ->
             let x20 = 1 in 
             let x19 = () in 
             let k16 q15 acc15 x18 =
               let f4 = fun k17 q16 acc16 y ->
                          let x21 = x + y in  k17 q16 acc16 x21 in 
               let x17 = x19 ; f4 in  k15 q15 acc15 x17 in 
             ev_assert k16 q14 acc14 x20 in 
  let k3 q2 acc2 res0 =
    let k18 q acc x22 =
      let x24 = 40 in 
      let x23 = acc = x24 in  assert(x23);x22 in 
    k18 q2 acc2 res0 in 
  f0 k3 q1 acc1 f3