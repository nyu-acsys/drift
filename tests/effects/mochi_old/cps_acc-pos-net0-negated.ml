(* CPS conversion. Source Program: 


let rec reduce n =
  ev -1;
  if n <= 0 then 0 else reduce (n - 1)

let main (mn:int(*-:{v:Int | true}*)) = 
  if mn > 0 then begin ev mn; reduce mn end else begin ev 1; 0 end


Property: 

(* 
    NEGATION(Accumulation stays positive, ends at 0)
    =
    there is at least one point where accumulation goes negative
         (tracked via sink state q1)
    \/ 
    accumulation ends at non-zero
*)

QSet   = [0;1]; 

delta  = fun evx (q, acc) -> 
    if (q = 1) then (1, acc+evx)
    else if (acc+evx < 0) then (1, acc+evx)
    else (0, acc+evx);

IniCfg = (0, 0); 

assertFinal = fun (q, acc) -> q=1 || acc<0 || acc>0;



*)

let main prefmn = 
  let ev = fun k0 q acc evx ->
             if (q = 1) then k0 1 (acc + evx) () 
             else if ((acc + evx) < 0) then k0 1 (acc + evx) () 
                  else k0 0 (acc + evx) () in 
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
                          let x10 = () in 
                          let k13 q12 acc12 x9 =
                            let k14 q13 acc13 res6 =
                              let x8 = x10 ; res6 in  k9 q13 acc13 x8 in 
                            reduce k14 q12 acc12 mn in 
                          ev k13 q9 acc9 mn in 
                        let k11 q10 acc10 res5 =
                          let x6 = 1 in 
                          let x5 = () in 
                          let k12 q11 acc11 x4 =
                            let x7 = 0 in 
                            let x3 = x5 ; x7 in  k9 q11 acc11 x3 in 
                          ev k12 q10 acc10 x6 in 
                        if x1 then k10 q7 acc7 x1 else k11 q7 acc7 x1 in 
             let k5 q4 acc4 res1 =
               k4 q4 acc4 res1 in 
             f1 k5 q3 acc3 f2 in 
  let rec reduce k15 q14 acc14 n =
    let x14 = -1 in 
    let x13 = () in 
    let k16 q15 acc15 x12 =
      let x16 = 0 in 
      let x15 = n <= x16 in 
      let k17 q16 acc16 res7 =
        let x11 = x13 ; res7 in  k15 q16 acc16 x11 in 
      let k18 q17 acc17 res8 =
        let x19 = 0 in 
        k17 q17 acc17 x19 in 
      let k19 q18 acc18 res9 =
        let x18 = 1 in 
        let x17 = n - x18 in  let k20 q19 acc19 res10 =
                                k17 q19 acc19 res10 in 
                              reduce k20 q18 acc18 x17 in 
      if x15 then k18 q15 acc15 x15 else k19 q15 acc15 x15 in 
    ev k16 q14 acc14 x14 in 
  let k3 q2 acc2 res0 =
    let k21 q acc x20 =
      let x24 = 1 in 
      let x23 = q = x24 in  let x26 = 0 in 
                            let x25 = acc < x26 in  let x22 = x23 || x25 in  let x28 = 0 in 
                                                                             let x27 = acc > x28 in  let x21 = x22 || x27 in  assert(x21);x20 in 
    k21 q2 acc2 res0 in 
  f0 k3 q1 acc1 reduce