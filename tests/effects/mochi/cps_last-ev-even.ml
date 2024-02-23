(* CPS conversion. Source Program: 

let rec foo vv f =
  if vv = 0 then ev f
  else begin ev vv; foo (vv-1) f end 

let main (v:int(*-:{v:Int | true}*)) (final:int(*-:{v:Int | true}*)) =
  if (final mod 2 = 0) then 
    foo v final
  else 
    foo v (if v >= 0 then final+1 else final) 
  


Property: 

(* Last event seen is even *)

QSet   = [0]; 

delta  = fun evx (q, acc) -> (q,evx);

IniCfg = (0, 0);

assertFinal = fun (q, acc) -> acc mod 2 = 0;


*)

let main prefv preffinal = 
  let ev = fun k0 q acc evx ->
             k0 q evx () in 
  let q1 = 0 in 
  let acc1 = 0 in 
  let f0 = fun k4 q3 acc3 foo ->
             let f1 = fun k6 q5 acc5 _main ->
                        let k8 q7 acc7 res3 =
                          let k7 q6 acc6 res2 =
                            k6 q6 acc6 res2 in 
                          res3 k7 q7 acc7 preffinal in 
                        _main k8 q5 acc5 prefv in 
             let f2 = fun k9 q8 acc8 v ->
                        let f3 = fun k10 q9 acc9 final ->
                                   let x3 = 2 in 
                                   let x2 = final mod x3 in 
                                   let x4 = 0 in 
                                   let x1 = x2 = x4 in 
                                   let k11 q10 acc10 res4 =
                                     k10 q10 acc10 res4 in 
                                   let k12 q11 acc11 res5 =
                                     let k20 q19 acc19 res13 =
                                       let k19 q18 acc18 res12 =
                                         k11 q18 acc18 res12 in 
                                       res13 k19 q19 acc19 final in 
                                     foo k20 q11 acc11 v in 
                                   let k13 q12 acc12 res6 =
                                     let k15 q14 acc14 res8 =
                                       let x6 = 0 in 
                                       let x5 = v >= x6 in 
                                       let k16 q15 acc15 res9 =
                                         let k14 q13 acc13 res7 =
                                           k11 q13 acc13 res7 in 
                                         res8 k14 q15 acc15 res9 in 
                                       let k17 q16 acc16 res10 =
                                         let x8 = 1 in 
                                         let x7 = final + x8 in  k16 q16 acc16 x7 in 
                                       let k18 q17 acc17 res11 =
                                         k16 q17 acc17 final in 
                                       if x5 then k17 q14 acc14 x5 else k18 q14 acc14 x5 in 
                                     foo k15 q12 acc12 v in 
                                   if x1 then k12 q9 acc9 x1 else k13 q9 acc9 x1 in 
                        k9 q8 acc8 f3 in 
             let k5 q4 acc4 res1 =
               k4 q4 acc4 res1 in 
             f1 k5 q3 acc3 f2 in 
  let rec foo k21 q20 acc20 vv =
    let f4 = fun k22 q21 acc21 f ->
               let x10 = 0 in 
               let x9 = vv = x10 in 
               let k23 q22 acc22 res14 =
                 k22 q22 acc22 res14 in 
               let k24 q23 acc23 res15 =
                 let x17 = () in 
                 let k29 q28 acc28 x16 =
                   k23 q28 acc28 x17 in 
                 ev k29 q23 acc23 f in 
               let k25 q24 acc24 res16 =
                 let x13 = () in 
                 let k26 q25 acc25 x12 =
                   let x15 = 1 in 
                   let x14 = vv - x15 in  let k28 q27 acc27 res18 =
                                            let k27 q26 acc26 res17 =
                                              let x11 = x13 ; res17 in  k23 q26 acc26 x11 in 
                                            res18 k27 q27 acc27 f in 
                                          foo k28 q25 acc25 x14 in 
                 ev k26 q24 acc24 vv in 
               if x9 then k24 q21 acc21 x9 else k25 q21 acc21 x9 in 
    k21 q20 acc20 f4 in 
  let k3 q2 acc2 res0 =
    let k30 q acc x18 =
      let x21 = 2 in 
      let x20 = acc mod x21 in  let x22 = 0 in 
                                let x19 = x20 = x22 in  assert(x19);x18 in 
    k30 q2 acc2 res0 in 
  f0 k3 q1 acc1 foo