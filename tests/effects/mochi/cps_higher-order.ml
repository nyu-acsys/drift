(* CPS conversion. Source Program: 

let rec apply f x = (ev 1; if x <= 0 then 0 else ((apply f (f x))))

let pred x = x - 1
let succ x = x + 1

let main (n:int(*-:{v:Int | true}*)) =
  let _ = apply pred n in
  ev 2; 0

Property: 

(*
   Traces are just [1;2]
   q0 --1--> q1 --2--> q2
   q3 is the error state
*)
QSet   = [0; 1; 2; 3]; 

delta  = fun evx (q, acc) -> 
  if      ((evx = 1) && (q = 0 || q = 1)) then (1,acc)
  else if ((evx = 2) && (q = 1)) then (2,acc)
  else (3,0);

IniCfg = (0, 0);

assertFinal = fun (q, acc) -> q = 2;


*)

let main prefn = 
  let ev = fun k0 q acc evx ->
             if ((evx = 1) && ((q = 0) || (q = 1))) then k0 1 acc () 
             else if ((evx = 2) && (q = 1)) then k0 2 acc () 
                  else k0 3 0 () in 
  let q1 = 0 in 
  let acc1 = 0 in 
  let f0 = fun k4 q3 acc3 apply ->
             let f1 = fun k6 q5 acc5 pred ->
                        let f2 = fun k8 q7 acc7 succ ->
                                   let f3 = fun k10 q9 acc9 _main ->
                                              let k11 q10 acc10 res4 =
                                                k10 q10 acc10 res4 in 
                                              _main k11 q9 acc9 prefn in 
                                   let f4 = fun k12 q11 acc11 n ->
                                              let f5 = fun k14 q13 acc13 _ ->
                                                         let x4 = 2 in 
                                                         let x3 = () in 
                                                         let k15 q14 acc14 x2 =
                                                           let x5 = 0 in 
                                                           let x1 = x3 ; x5 in  k14 q14 acc14 x1 in 
                                                         ev k15 q13 acc13 x4 in 
                                              let k17 q16 acc16 res7 =
                                                let k16 q15 acc15 res6 =
                                                  let k13 q12 acc12 res5 =
                                                    k12 q12 acc12 res5 in 
                                                  f5 k13 q15 acc15 res6 in 
                                                res7 k16 q16 acc16 n in 
                                              apply k17 q11 acc11 pred in 
                                   let k9 q8 acc8 res3 =
                                     k8 q8 acc8 res3 in 
                                   f3 k9 q7 acc7 f4 in 
                        let f6 = fun k18 q17 acc17 x ->
                                   let x7 = 1 in 
                                   let x6 = x + x7 in  k18 q17 acc17 x6 in 
                        let k7 q6 acc6 res2 =
                          k6 q6 acc6 res2 in 
                        f2 k7 q5 acc5 f6 in 
             let f7 = fun k19 q18 acc18 x ->
                        let x9 = 1 in 
                        let x8 = x - x9 in  k19 q18 acc18 x8 in 
             let k5 q4 acc4 res1 =
               k4 q4 acc4 res1 in 
             f1 k5 q3 acc3 f7 in 
  let rec apply k20 q19 acc19 f =
    let f8 = fun k21 q20 acc20 x ->
               let x13 = 1 in 
               let x12 = () in 
               let k22 q21 acc21 x11 =
                 let x15 = 0 in 
                 let x14 = x <= x15 in 
                 let k23 q22 acc22 res8 =
                   let x10 = x12 ; res8 in  k21 q22 acc22 x10 in 
                 let k24 q23 acc23 res9 =
                   let x16 = 0 in 
                   k23 q23 acc23 x16 in 
                 let k25 q24 acc24 res10 =
                   let k27 q26 acc26 res12 =
                     let k28 q27 acc27 res13 =
                       let k26 q25 acc25 res11 =
                         k23 q25 acc25 res11 in 
                       res12 k26 q27 acc27 res13 in 
                     f k28 q26 acc26 x in 
                   apply k27 q24 acc24 f in 
                 if x14 then k24 q21 acc21 x14 else k25 q21 acc21 x14 in 
               ev k22 q20 acc20 x13 in 
    k20 q19 acc19 f8 in 
  let k3 q2 acc2 res0 =
    let k29 q acc x17 =
      let x19 = 2 in 
      let x18 = q = x19 in  assert(x18);x17 in 
    k29 q2 acc2 res0 in 
  f0 k3 q1 acc1 apply