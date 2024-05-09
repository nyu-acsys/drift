(* CPS conversion. Source Program: 

let rec apply f x = if x <= 0 then 0 else apply f ((f x)) 
let tick c t = ev (if t mod 2 = 0 then c else (-c)); t-2 
let main (n: int(*-:{v:Int | v > 0}*)) (x: int(*-:{v:Int | v > 0 }*)) = 
  apply (tick x) n


(* 
let rec apply q acc f x = 
  if x <= 0 then (0, (q, acc))  
  else 
    let (x', (q', acc')) = f q acc x in
    apply q' acc' x'

let tick q acc c t = 
  let (q', acc') = ev_aut_step q acc (if t mod 2 then x else (-c)) in
  assert (q' = 1 || q' = 2); 
  (t-2, (q', acc'))

let main (n:int) (c:int) = 
  let (q, acc) = (0,0) in
  if (n > 0 && c > 0) then
    apply 
*)


Property: 

(* Disjunctive *)

QSet   = [0;1;2;3]; 

delta  = fun evx (q, acc) -> 
       	     if (q = 0) && (evx > 0) then (1,evx)
	     else if (q = 1) && (evx < 0) then (3, evx)
       	     else if (q = 0) && (evx < 0) then (2,evx)
	     else if (q = 2) && (evx > 0) then (3, evx)
	     else if (q < 3) && (acc <> evx) then (3, evx)
	     else (q, acc);

(* delta = fun evx (q, acc) -> 
      if (q = 0 && evx > 0) then 
      else if q = 0 && acc = 0 then (1, evx)
      else if q = 1 && acc = evx then (q, acc)
      else (2, acc); *)

(* delta = fun evx (q, acc) -> 
      if q = 2 then (q, acc)
      else if q = 0 && acc = 0 then (1, evx)
      else if q = 1 && acc = evx then (q, acc)
      else (2, acc); *)

(* delta = fun evx (q, acc) -> 
      if (q = 1) then (1, acc)
      else if acc = 0 then (0, evx)
      else if acc = evx then (q, acc)
      else (1, acc); *)

IniCfg = (0, 0);

(* if event is pos, x must  be even ; *)
assert = fun (q, acc) -> q = 1 || q = 2;



*)

let main prefx prefn = 
  let ev = fun k0 q acc evx ->
             if ((q = 0) && (evx > 0)) then k0 1 evx () 
             else if ((q = 1) && (evx < 0)) then k0 3 evx () 
                  else if ((q = 0) && (evx < 0)) then k0 2 evx () 
                       else if ((q = 2) && (evx > 0)) then k0 3 evx () 
                            else if ((q < 3) && (acc != evx)) then k0 3 evx () 
                                 else k0 q acc () in 
  let ev_assert = fun k1 q0 acc0 x0 ->
                    let k30 q acc x15 =
                      let x18 = 1 in 
                      let x17 = q = x18 in  let x20 = 2 in 
                                            let x19 = q = x20 in  let x16 = x17 || x19 in  let x14 = () in 
                                                                                           assert(x16);k1 q acc x14 in 
                    ev k30 q0 acc0 x0 in 
  let q1 = 0 in 
  let acc1 = 0 in 
  let f0 = fun k4 q3 acc3 apply ->
             let f1 = fun k6 q5 acc5 tick ->
                        let f2 = fun k8 q7 acc7 _main ->
                                   let k10 q9 acc9 res4 =
                                     let k9 q8 acc8 res3 =
                                       k8 q8 acc8 res3 in 
                                     res4 k9 q9 acc9 prefx in 
                                   _main k10 q7 acc7 prefn in 
                        let f3 = fun k11 q10 acc10 n ->
                                   let f4 = fun k12 q11 acc11 x ->
                                              let k15 q14 acc14 res7 =
                                                let k14 q13 acc13 res6 =
                                                  let k13 q12 acc12 res5 =
                                                    k12 q12 acc12 res5 in 
                                                  res6 k13 q13 acc13 n in 
                                                apply k14 q14 acc14 res7 in 
                                              tick k15 q11 acc11 x in 
                                   k11 q10 acc10 f4 in 
                        let k7 q6 acc6 res2 =
                          k6 q6 acc6 res2 in 
                        f2 k7 q5 acc5 f3 in 
             let f5 = fun k16 q15 acc15 c ->
                        let f6 = fun k17 q16 acc16 t ->
                                   let x6 = 2 in 
                                   let x5 = t mod x6 in 
                                   let x7 = 0 in 
                                   let x4 = x5 = x7 in 
                                   let k19 q18 acc18 res8 =
                                     let x3 = () in 
                                     let k18 q17 acc17 x2 =
                                       let x10 = 2 in 
                                       let x9 = t - x10 in  let x1 = x3 ; x9 in  k17 q17 acc17 x1 in 
                                     ev_assert k18 q18 acc18 res8 in 
                                   let k20 q19 acc19 res9 =
                                     k19 q19 acc19 c in 
                                   let k21 q20 acc20 res10 =
                                     let x8 = - c in  k19 q20 acc20 x8 in 
                                   if x4 then k20 q16 acc16 x4 else k21 q16 acc16 x4 in 
                        k16 q15 acc15 f6 in 
             let k5 q4 acc4 res1 =
               k4 q4 acc4 res1 in 
             f1 k5 q3 acc3 f5 in 
  let rec apply k22 q21 acc21 f =
    let f7 = fun k23 q22 acc22 x ->
               let x12 = 0 in 
               let x11 = x <= x12 in 
               let k24 q23 acc23 res11 =
                 k23 q23 acc23 res11 in 
               let k25 q24 acc24 res12 =
                 let x13 = 0 in 
                 k24 q24 acc24 x13 in 
               let k26 q25 acc25 res13 =
                 let k28 q27 acc27 res15 =
                   let k29 q28 acc28 res16 =
                     let k27 q26 acc26 res14 =
                       k24 q26 acc26 res14 in 
                     res15 k27 q28 acc28 res16 in 
                   f k29 q27 acc27 x in 
                 apply k28 q25 acc25 f in 
               if x11 then k25 q22 acc22 x11 else k26 q22 acc22 x11 in 
    k22 q21 acc21 f7 in 
  let k3 q2 acc2 res0 =
    res0 in 
  f0 k3 q1 acc1 apply