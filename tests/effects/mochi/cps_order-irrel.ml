(* CPS conversion. Source Program: 

(* 
there are two particular events (let us treat them as integers
 c and -c for some c) such that at most one of them is permitted 
 to occur during any execution
*)
let rec order d c = 
  if (d > 0) then begin
     (* begin if ( d mod 2 = 0 ) then ev c else ev(-c) end; *)
     ev (if d mod 2 = 0 then c else (-c)); 
     order (d - 2) c
  end else 0

let main (dd:int(*-:{v:Int | true}*)) (cc:int(*-:{v:Int | v > 0}*)) = 
  order dd cc



Property: 

(* there are two particular events (let us treat them as integers
 c and -c for some c) such that at most one of them is permitted 
 to occur during any execution *)

QSet   = [0;1]; 

delta  = fun evx (q, acc) -> 
    if q = 1 then (1, acc)
    else if acc = 0 then (q, evx) 
    else if acc = evx then (q, acc)
    else (1,acc);

IniCfg = (0, 0);

assertFinal = fun (q, acc) -> q = 0;


*)

let main prefdd prefcc = 
  let ev = fun k0 q acc evx ->
             if (q = 1) then k0 1 acc () 
             else if (acc = 0) then k0 q evx () 
                  else if (acc = evx) then k0 q acc () 
                       else k0 1 acc () in 
  let q1 = 0 in 
  let acc1 = 0 in 
  let f0 = fun k4 q3 acc3 order ->
             let f1 = fun k6 q5 acc5 _main ->
                        let k8 q7 acc7 res3 =
                          let k7 q6 acc6 res2 =
                            k6 q6 acc6 res2 in 
                          res3 k7 q7 acc7 prefcc in 
                        _main k8 q5 acc5 prefdd in 
             let f2 = fun k9 q8 acc8 dd ->
                        let f3 = fun k10 q9 acc9 cc ->
                                   let k12 q11 acc11 res5 =
                                     let k11 q10 acc10 res4 =
                                       k10 q10 acc10 res4 in 
                                     res5 k11 q11 acc11 cc in 
                                   order k12 q9 acc9 dd in 
                        k9 q8 acc8 f3 in 
             let k5 q4 acc4 res1 =
               k4 q4 acc4 res1 in 
             f1 k5 q3 acc3 f2 in 
  let rec order k13 q12 acc12 d =
    let f4 = fun k14 q13 acc13 c ->
               let x2 = 0 in 
               let x1 = d > x2 in 
               let k15 q14 acc14 res6 =
                 k14 q14 acc14 res6 in 
               let k16 q15 acc15 res7 =
                 let x9 = 2 in 
                 let x8 = d mod x9 in 
                 let x10 = 0 in 
                 let x7 = x8 = x10 in 
                 let k19 q18 acc18 res9 =
                   let x6 = () in 
                   let k18 q17 acc17 x5 =
                     let x13 = 2 in 
                     let x12 = d - x13 in  let k23 q22 acc22 res13 =
                                             let k22 q21 acc21 res12 =
                                               let x4 = x6 ; res12 in  k15 q21 acc21 x4 in 
                                             res13 k22 q22 acc22 c in 
                                           order k23 q17 acc17 x12 in 
                   ev k18 q18 acc18 res9 in 
                 let k20 q19 acc19 res10 =
                   k19 q19 acc19 c in 
                 let k21 q20 acc20 res11 =
                   let x11 = - c in  k19 q20 acc20 x11 in 
                 if x7 then k20 q15 acc15 x7 else k21 q15 acc15 x7 in 
               let k17 q16 acc16 res8 =
                 let x3 = 0 in 
                 k15 q16 acc16 x3 in 
               if x1 then k16 q13 acc13 x1 else k17 q13 acc13 x1 in 
    k13 q12 acc12 f4 in 
  let k3 q2 acc2 res0 =
    let k24 q acc x14 =
      let x16 = 0 in 
      let x15 = q = x16 in  assert(x15);x14 in 
    k24 q2 acc2 res0 in 
  f0 k3 q1 acc1 order