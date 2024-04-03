(* CPS conversion. Source Program: 

let main (u: unit(*-:{v:Unit | unit}*)) = 
  ev 1; 0


Property: 


QSet   = [0]; 

delta  = fun evx (q, acc) -> (q, acc);

IniCfg = (0, 0);

assert = fun (q, acc) -> true;

*)

let main prefu = 
  let ev = fun k0 q acc evx ->
             k0 q acc () in 
  let ev_assert = fun k1 q0 acc0 x0 ->
                    let k8 q acc x7 =
                      let x8 = true in 
                      let x6 = () in 
                      assert(x8);k1 q acc x6 in 
                    ev k8 q0 acc0 x0 in 
  let q1 = 0 in 
  let acc1 = 0 in 
  let f0 = fun k4 q3 acc3 _main ->
             let k5 q4 acc4 res1 =
               k4 q4 acc4 res1 in 
             _main k5 q3 acc3 prefu in 
  let f1 = fun k6 q5 acc5 u ->
             let x4 = 1 in 
             let x3 = () in 
             let k7 q6 acc6 x2 =
               let x5 = 0 in 
               let x1 = x3 ; x5 in  k6 q6 acc6 x1 in 
             ev_assert k7 q5 acc5 x4 in 
  let k3 q2 acc2 res0 =
    res0 in 
  f0 k3 q1 acc1 f1