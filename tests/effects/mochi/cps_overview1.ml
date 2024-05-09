(* CPS conversion. Source Program: 

(* 
 there are two particular events
 (let us treat them as integers c and -c for some c)
 and a temporal ordering:
 q0 --(c)--> q1 --(-c)--> q2
 where q0 and q1 are error states
*)

let rec busy n t =
  if n <= 0 then begin 
    ev -t; 
    0
  end else busy (n - 1) t

let main (x:int(*-:{v:Int | true}*)) (n:int(*-:{v:Int | true}*)) = 
  ev x;
  busy n x


(*
if dd > 0 then begin ev dd; ev(-dd) end
  else begin ev(-dd); ev dd; end
*)



Property: 

QSet   = [0;1;2]; 

delta  = fun evx (q, acc) -> 
    if (q = 0) then (1, evx)
    else if (q = 1) && (acc + evx = 0) then (2, acc)
    else if (q = 2) then (2, acc)
    else (q, acc);

IniCfg = (0, 0);

assertFinal = fun (q, acc) -> q = 2;


*)

let main prefx prefn = 
  let ev = fun k0 q acc evx ->
             if (q = 0) then k0 1 evx () 
             else if ((q = 1) && ((acc + evx) = 0)) then k0 2 acc () 
                  else if (q = 2) then k0 2 acc () 
                       else k0 q acc () in 
  let q1 = 0 in 
  let acc1 = 0 in 
  let f0 = fun k4 q3 acc3 busy ->
             let f1 = fun k6 q5 acc5 _main ->
                        let k8 q7 acc7 res3 =
                          let k7 q6 acc6 res2 =
                            k6 q6 acc6 res2 in 
                          res3 k7 q7 acc7 prefn in 
                        _main k8 q5 acc5 prefx in 
             let f2 = fun k9 q8 acc8 x ->
                        let f3 = fun k10 q9 acc9 n ->
                                   let x3 = () in 
                                   let k11 q10 acc10 x2 =
                                     let k13 q12 acc12 res5 =
                                       let k12 q11 acc11 res4 =
                                         let x1 = x3 ; res4 in  k10 q11 acc11 x1 in 
                                       res5 k12 q12 acc12 x in 
                                     busy k13 q10 acc10 n in 
                                   ev k11 q9 acc9 x in 
                        k9 q8 acc8 f3 in 
             let k5 q4 acc4 res1 =
               k4 q4 acc4 res1 in 
             f1 k5 q3 acc3 f2 in 
  let rec busy k14 q13 acc13 n =
    let f4 = fun k15 q14 acc14 t ->
               let x5 = 0 in 
               let x4 = n <= x5 in 
               let k16 q15 acc15 res6 =
                 k15 q15 acc15 res6 in 
               let k17 q16 acc16 res7 =
                 let x11 = - t in  let x10 = () in 
                                   let k21 q20 acc20 x9 =
                                     let x12 = 0 in 
                                     let x8 = x10 ; x12 in  k16 q20 acc20 x8 in 
                                   ev k21 q16 acc16 x11 in 
               let k18 q17 acc17 res8 =
                 let x7 = 1 in 
                 let x6 = n - x7 in  let k20 q19 acc19 res10 =
                                       let k19 q18 acc18 res9 =
                                         k16 q18 acc18 res9 in 
                                       res10 k19 q19 acc19 t in 
                                     busy k20 q17 acc17 x6 in 
               if x4 then k17 q14 acc14 x4 else k18 q14 acc14 x4 in 
    k14 q13 acc13 f4 in 
  let k3 q2 acc2 res0 =
    let k22 q acc x13 =
      let x15 = 2 in 
      let x14 = q = x15 in  assert(x14);x13 in 
    k22 q2 acc2 res0 in 
  f0 k3 q1 acc1 busy