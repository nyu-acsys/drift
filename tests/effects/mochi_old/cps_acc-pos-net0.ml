(* CPS conversion. Source Program: 


let rec reduce n =
  ev -1;
  if n <= 0 then 0 else begin reduce (n - 1) end

let main (mn:int(*-:{v:Int | true}*)) = 
  if mn > 0 then begin ev mn; reduce (mn-1) end else 0


Property: 

(* Accumulation stays positive, ends at 0 *)

QSet   = [0]; 

delta  = fun evx (q, acc) -> (q,acc + evx);

IniCfg = (0, 0); 

assert = fun (q, acc) -> (acc >= 0);

assertFinal = fun (q, acc) -> (acc = 0);



*)

let main prefmn = 
  let ev = fun k0 q acc evx ->
             k0 q (acc + evx) () in 
  let ev_assert = fun k1 q0 acc0 x0 ->
                    let k21 q acc x22 =
                      let x24 = 0 in 
                      let x23 = acc >= x24 in  let x21 = () in 
                                               assert(x23);k1 q acc x21 in 
                    ev k21 q0 acc0 x0 in 
  let q1 = 0 in 
  let acc1 = 0 in 
  let f0 = fun k4 q3 acc3 reduce ->
             let f1 = fun k6 q5 acc5 _main ->
                        let k7 q6 acc6 res2 =
                          k6 q6 acc6 res2 in 
                        _main k7 q5 acc5 prefmn in 
             let f2 = fun k8 q7 acc7 mn ->
                        let x2 = 0 in 
                        let x1 = mn > x2 in 
                        let k9 q8 acc8 res3 =
                          k8 q8 acc8 res3 in 
                        let k10 q9 acc9 res4 =
                          let x6 = () in 
                          let k12 q11 acc11 x5 =
                            let x8 = 1 in 
                            let x7 = mn - x8 in  let k13 q12 acc12 res6 =
                                                   let x4 = x6 ; res6 in  k9 q12 acc12 x4 in 
                                                 reduce k13 q11 acc11 x7 in 
                          ev_assert k12 q9 acc9 mn in 
                        let k11 q10 acc10 res5 =
                          let x3 = 0 in 
                          k9 q10 acc10 x3 in 
                        if x1 then k10 q7 acc7 x1 else k11 q7 acc7 x1 in 
             let k5 q4 acc4 res1 =
               k4 q4 acc4 res1 in 
             f1 k5 q3 acc3 f2 in 
  let rec reduce k14 q13 acc13 n =
    let x12 = -1 in 
    let x11 = () in 
    let k15 q14 acc14 x10 =
      let x14 = 0 in 
      let x13 = n <= x14 in 
      let k16 q15 acc15 res7 =
        let x9 = x11 ; res7 in  k14 q15 acc15 x9 in 
      let k17 q16 acc16 res8 =
        let x17 = 0 in 
        k16 q16 acc16 x17 in 
      let k18 q17 acc17 res9 =
        let x16 = 1 in 
        let x15 = n - x16 in  let k19 q18 acc18 res10 =
                                k16 q18 acc18 res10 in 
                              reduce k19 q17 acc17 x15 in 
      if x13 then k17 q14 acc14 x13 else k18 q14 acc14 x13 in 
    ev_assert k15 q13 acc13 x12 in 
  let k3 q2 acc2 res0 =
    let k20 q acc x18 =
      let x20 = 0 in 
      let x19 = acc = x20 in  assert(x19);x18 in 
    k20 q2 acc2 res0 in 
  f0 k3 q1 acc1 reduce