(* CPS conversion. Source Program: 

let rec spend n =
  ev -1;
  if n <= 0 then 0 else spend (n - 1)

let main (gas:int(*-:{v:Int | true}*)) (n:int(*-:{v:Int | true}*)) = 
  if (gas > n && n >= 0) then begin ev gas; spend n end else 0


Property: 

(* Resource Analysis *)

QSet   = [0]; 

delta  = fun evx (q, acc) -> (q,acc + evx);

IniCfg = (0, 0);

assert = fun (q, acc) -> (acc >= 0);


*)

let main prefn prefgas = 
  let ev = fun k0 q acc evx ->
             k0 q (acc + evx) () in 
  let ev_assert = fun k1 q0 acc0 x0 ->
                    let k22 q acc x19 =
                      let x21 = 0 in 
                      let x20 = acc >= x21 in  let x18 = () in 
                                               assert(x20);k1 q acc x18 in 
                    ev k22 q0 acc0 x0 in 
  let q1 = 0 in 
  let acc1 = 0 in 
  let f0 = fun k4 q3 acc3 spend ->
             let f1 = fun k6 q5 acc5 _main ->
                        let k8 q7 acc7 res3 =
                          let k7 q6 acc6 res2 =
                            k6 q6 acc6 res2 in 
                          res3 k7 q7 acc7 prefn in 
                        _main k8 q5 acc5 prefgas in 
             let f2 = fun k9 q8 acc8 gas ->
                        let f3 = fun k10 q9 acc9 n ->
                                   let x2 = gas > n in 
                                   let x4 = 0 in 
                                   let x3 = n >= x4 in 
                                   let x1 = x2 && x3 in 
                                   let k11 q10 acc10 res4 =
                                     k10 q10 acc10 res4 in 
                                   let k12 q11 acc11 res5 =
                                     let x8 = () in 
                                     let k14 q13 acc13 x7 =
                                       let k15 q14 acc14 res7 =
                                         let x6 = x8 ; res7 in  k11 q14 acc14 x6 in 
                                       spend k15 q13 acc13 n in 
                                     ev_assert k14 q11 acc11 gas in 
                                   let k13 q12 acc12 res6 =
                                     let x5 = 0 in 
                                     k11 q12 acc12 x5 in 
                                   if x1 then k12 q9 acc9 x1 else k13 q9 acc9 x1 in 
                        k9 q8 acc8 f3 in 
             let k5 q4 acc4 res1 =
               k4 q4 acc4 res1 in 
             f1 k5 q3 acc3 f2 in 
  let rec spend k16 q15 acc15 n =
    let x12 = -1 in 
    let x11 = () in 
    let k17 q16 acc16 x10 =
      let x14 = 0 in 
      let x13 = n <= x14 in 
      let k18 q17 acc17 res8 =
        let x9 = x11 ; res8 in  k16 q17 acc17 x9 in 
      let k19 q18 acc18 res9 =
        let x17 = 0 in 
        k18 q18 acc18 x17 in 
      let k20 q19 acc19 res10 =
        let x16 = 1 in 
        let x15 = n - x16 in  let k21 q20 acc20 res11 =
                                k18 q20 acc20 res11 in 
                              spend k21 q19 acc19 x15 in 
      if x13 then k19 q16 acc16 x13 else k20 q16 acc16 x13 in 
    ev_assert k17 q15 acc15 x12 in 
  let k3 q2 acc2 res0 =
    res0 in 
  f0 k3 q1 acc1 spend