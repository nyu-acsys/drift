(* CPS conversion. Source Program: 




let main (v:int(*-:{v:Int | true}*)) =
  if v = 0 then begin ev 1; 0 end else 0


Property: 

(* Dependent *)

QSet   = [0]; 

delta  = fun evx (q, acc) -> 
   if evx > prefv then (q,acc + evx) else (q, acc);

IniCfg = (0, 0);

assert = fun (q, acc) -> (acc >= 0);



*)

let main prefv = 
  let ev = fun k0 q acc evx ->
             if (evx > prefv) then k0 q (acc + evx) () 
             else k0 q acc () in 
  let ev_assert = fun k1 q0 acc0 x0 ->
                    let k11 q acc x10 =
                      let x12 = 0 in 
                      let x11 = acc >= x12 in  let x9 = () in 
                                               assert(x11);k1 q acc x9 in 
                    ev k11 q0 acc0 x0 in 
  let q1 = 0 in 
  let acc1 = 0 in 
  let f0 = fun k4 q3 acc3 _main ->
             let k5 q4 acc4 res1 =
               k4 q4 acc4 res1 in 
             _main k5 q3 acc3 prefv in 
  let f1 = fun k6 q5 acc5 v ->
             let x2 = 0 in 
             let x1 = v = x2 in 
             let k7 q6 acc6 res2 =
               k6 q6 acc6 res2 in 
             let k8 q7 acc7 res3 =
               let x7 = 1 in 
               let x6 = () in 
               let k10 q9 acc9 x5 =
                 let x8 = 0 in 
                 let x4 = x6 ; x8 in  k7 q9 acc9 x4 in 
               ev_assert k10 q7 acc7 x7 in 
             let k9 q8 acc8 res4 =
               let x3 = 0 in 
               k7 q8 acc8 x3 in 
             if x1 then k8 q5 acc5 x1 else k9 q5 acc5 x1 in 
  let k3 q2 acc2 res0 =
    res0 in 
  f0 k3 q1 acc1 f1