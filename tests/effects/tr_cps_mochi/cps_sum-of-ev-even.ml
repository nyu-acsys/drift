(* CPS conversion. Source Program: 

let rec sum x = 
  ev x;
  if x <= 0 then 0 else sum (x - 2)

let main (v:int(*-:{v:Int | true}*)) =
  if v mod 2 = 0 then sum v else sum (v + 1)


Property: 

(* Accumulate the Sum, and Sum is Even *)

QSet   = [0]; 

delta  = fun evx (q, acc) -> (q,acc + evx);

IniCfg = (0, 0);

assert = fun (q, acc) -> true;
 
assertFinal = fun (q, acc) -> (acc mod 2 = 0);



*)

let main prefv = 
  let ev = fun k0 q acc evx ->
             k0 q (acc + evx) () in 
  let ev_assert = fun k1 q0 acc0 x0 ->
                    let k21 q acc x21 =
                      let x22 = true in 
                      let x20 = () in 
                      assert(x22);k1 q acc x20 in 
                    ev k21 q0 acc0 x0 in 
  let q1 = 0 in 
  let acc1 = 0 in 
  let f0 = fun k4 q3 acc3 sum ->
             let f1 = fun k6 q5 acc5 _main ->
                        let k7 q6 acc6 res2 =
                          k6 q6 acc6 res2 in 
                        _main k7 q5 acc5 prefv in 
             let f2 = fun k8 q7 acc7 v ->
                        let x3 = 2 in 
                        let x2 = v mod x3 in 
                        let x4 = 0 in 
                        let x1 = x2 = x4 in 
                        let k9 q8 acc8 res3 =
                          k8 q8 acc8 res3 in 
                        let k10 q9 acc9 res4 =
                          let k13 q12 acc12 res7 =
                            k9 q12 acc12 res7 in 
                          sum k13 q9 acc9 v in 
                        let k11 q10 acc10 res5 =
                          let x6 = 1 in 
                          let x5 = v + x6 in  let k12 q11 acc11 res6 =
                                                k9 q11 acc11 res6 in 
                                              sum k12 q10 acc10 x5 in 
                        if x1 then k10 q7 acc7 x1 else k11 q7 acc7 x1 in 
             let k5 q4 acc4 res1 =
               k4 q4 acc4 res1 in 
             f1 k5 q3 acc3 f2 in 
  let rec sum k14 q13 acc13 x =
    let x9 = () in 
    let k15 q14 acc14 x8 =
      let x11 = 0 in 
      let x10 = x <= x11 in 
      let k16 q15 acc15 res8 =
        let x7 = x9 ; res8 in  k14 q15 acc15 x7 in 
      let k17 q16 acc16 res9 =
        let x14 = 0 in 
        k16 q16 acc16 x14 in 
      let k18 q17 acc17 res10 =
        let x13 = 2 in 
        let x12 = x - x13 in  let k19 q18 acc18 res11 =
                                k16 q18 acc18 res11 in 
                              sum k19 q17 acc17 x12 in 
      if x10 then k17 q14 acc14 x10 else k18 q14 acc14 x10 in 
    ev_assert k15 q13 acc13 x in 
  let k3 q2 acc2 res0 =
    let k20 q acc x15 =
      let x18 = 2 in 
      let x17 = acc mod x18 in  let x19 = 0 in 
                                let x16 = x17 = x19 in  assert(x16);x15 in 
    k20 q2 acc2 res0 in 
  f0 k3 q1 acc1 sum