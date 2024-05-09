(* CPS conversion. Source Program: 

(*
 ./drift.exe -prop tests/effects/higher-order.yml.prp -file tests/effects/higher-order.ml -domain Polka_st -sen 1 -ev-trans false -trace-len 1 -if-part true
 *)
let caller f =
  ev (f 1)

let main n =
begin
  caller (fun x -> x); ev 2
end


Property: 

(*
   Traces are just [1;2]
   q0 --1--> q1 --2--> q2
   q3 is the error state
*)
QSet   = [0; 1; 2; 3]; 

delta  = fun evx (q, acc) -> 
  if      ((evx = 1) && (q = 0)) then (1,acc)
  else if ((evx = 2) && (q = 1)) then (2,acc)
  else (3,0);

IniCfg = (0, 0);

assertFinal = fun (q, acc) -> q = 2;


*)

let main  = 
  let ev = fun k0 q acc evx ->
             if ((evx = 1) && (q = 0)) then k0 1 acc () 
             else if ((evx = 2) && (q = 1)) then k0 2 acc () 
                  else k0 3 0 () in 
  let q1 = 0 in 
  let acc1 = 0 in 
  let f0 = fun k4 q3 acc3 caller ->
             let f1 = fun k6 q5 acc5 _main ->
                        let k7 q6 acc6 res2 =
                          k6 q6 acc6 res2 in 
                        _main k7 q5 acc5 prefn in 
             let f2 = fun k8 q7 acc7 n ->
                        let f3 = fun k10 q9 acc9 x ->
                                   k10 q9 acc9 x in 
                        let k9 q8 acc8 res3 =
                          let x4 = 2 in 
                          let x3 = () in 
                          let k11 q10 acc10 x2 =
                            let x1 = res3 ; x3 in  k8 q10 acc10 x1 in 
                          ev k11 q8 acc8 x4 in 
                        caller k9 q7 acc7 f3 in 
             let k5 q4 acc4 res1 =
               k4 q4 acc4 res1 in 
             f1 k5 q3 acc3 f2 in 
  let f4 = fun k12 q11 acc11 f ->
             let x7 = 1 in 
             let k14 q13 acc13 res4 =
               let x6 = () in 
               let k13 q12 acc12 x5 =
                 k12 q12 acc12 x6 in 
               ev k13 q13 acc13 res4 in 
             f k14 q11 acc11 x7 in 
  let k3 q2 acc2 res0 =
    let k15 q acc x8 =
      let x10 = 2 in 
      let x9 = q = x10 in  assert(x9);x8 in 
    k15 q2 acc2 res0 in 
  f0 k3 q1 acc1 f4