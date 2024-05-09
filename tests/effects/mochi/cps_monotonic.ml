(* CPS conversion. Source Program: 


let rec mono t x =
  ev t;
  if x <= 0 then 0
  else
    mono (t+x) (x-1) (* increase t by positive amount *)

let main (u: int(*-:{v:Int | true}*)) = 
  if u>0 then mono 1 u else 0



Property: 

(*
   Event values increase [1;2;5;...]
   Save previous value
   q1 is the error state
*)
QSet   = [0;1]; 

delta  = fun evx (q, acc) -> 
  if   (q = 0 && evx >= acc) then (0, evx)
  else (1, 0);

IniCfg = (0, 0);

assertFinal = fun (q, acc) -> q = 0;


*)

let main prefu = 
  let ev = fun k0 q acc evx ->
             if ((q = 0) && (evx >= acc)) then k0 0 evx () 
             else k0 1 0 () in 
  let q1 = 0 in 
  let acc1 = 0 in 
  let f0 = fun k4 q3 acc3 mono ->
             let f1 = fun k6 q5 acc5 _main ->
                        let k7 q6 acc6 res2 =
                          k6 q6 acc6 res2 in 
                        _main k7 q5 acc5 prefu in 
             let f2 = fun k8 q7 acc7 u ->
                        let x2 = 0 in 
                        let x1 = u > x2 in 
                        let k9 q8 acc8 res3 =
                          k8 q8 acc8 res3 in 
                        let k10 q9 acc9 res4 =
                          let x4 = 1 in 
                          let k13 q12 acc12 res7 =
                            let k12 q11 acc11 res6 =
                              k9 q11 acc11 res6 in 
                            res7 k12 q12 acc12 u in 
                          mono k13 q9 acc9 x4 in 
                        let k11 q10 acc10 res5 =
                          let x3 = 0 in 
                          k9 q10 acc10 x3 in 
                        if x1 then k10 q7 acc7 x1 else k11 q7 acc7 x1 in 
             let k5 q4 acc4 res1 =
               k4 q4 acc4 res1 in 
             f1 k5 q3 acc3 f2 in 
  let rec mono k14 q13 acc13 t =
    let f3 = fun k15 q14 acc14 x ->
               let x7 = () in 
               let k16 q15 acc15 x6 =
                 let x9 = 0 in 
                 let x8 = x <= x9 in 
                 let k17 q16 acc16 res8 =
                   let x5 = x7 ; res8 in  k15 q16 acc16 x5 in 
                 let k18 q17 acc17 res9 =
                   let x13 = 0 in 
                   k17 q17 acc17 x13 in 
                 let k19 q18 acc18 res10 =
                   let x10 = t + x in 
                   let k21 q20 acc20 res12 =
                     let x12 = 1 in 
                     let x11 = x - x12 in  let k20 q19 acc19 res11 =
                                             k17 q19 acc19 res11 in 
                                           res12 k20 q20 acc20 x11 in 
                   mono k21 q18 acc18 x10 in 
                 if x8 then k18 q15 acc15 x8 else k19 q15 acc15 x8 in 
               ev k16 q14 acc14 t in 
    k14 q13 acc13 f3 in 
  let k3 q2 acc2 res0 =
    let k22 q acc x14 =
      let x16 = 0 in 
      let x15 = q = x16 in  assert(x15);x14 in 
    k22 q2 acc2 res0 in 
  f0 k3 q1 acc1 mono