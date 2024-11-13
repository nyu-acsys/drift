(* CPS conversion. Source Program: 


let rec shrink t f d =
  if f () <= 0 then 0 
  else begin
    ev 1; (* ev[Shrink] *)
    let t' = f() - d in
    shrink t' (fun x -> t') d end

let main (gl_t:int(*-:{v:Int | true}*)) (gl_d:int(*-:{v:Int | true}*)) =   
    ev (gl_t/gl_d);
    shrink gl_t (fun x -> gl_t) gl_d


Property: 

(* Dependent *)
 
QSet   = [0]; 
 
delta  = fun evx (q, acc) -> (q, (acc+evx));
 
IniCfg = (0, 0);

assertFinal = fun (q, acc) -> (acc = prefgl_t/prefgl_d);
 


*)

let main prefgl_t prefgl_d = 
  let ev = fun k0 q acc evx ->
             k0 q (acc + evx) () in 
  let q1 = 0 in 
  let acc1 = 0 in 
  let f0 = fun k4 q3 acc3 shrink ->
             let f1 = fun k6 q5 acc5 _main ->
                        let k8 q7 acc7 res3 =
                          let k7 q6 acc6 res2 =
                            k6 q6 acc6 res2 in 
                          res3 k7 q7 acc7 prefgl_d in 
                        _main k8 q5 acc5 prefgl_t in 
             let f2 = fun k9 q8 acc8 gl_t ->
                        let f3 = fun k10 q9 acc9 gl_d ->
                                   let x4 = gl_t / gl_d in 
                                   let x3 = () in 
                                   let k11 q10 acc10 x2 =
                                     let k14 q13 acc13 res6 =
                                       let f4 = fun k15 q14 acc14 x ->
                                                  k15 q14 acc14 gl_t in 
                                       let k13 q12 acc12 res5 =
                                         let k12 q11 acc11 res4 =
                                           let x1 = x3 ; res4 in  k10 q11 acc11 x1 in 
                                         res5 k12 q12 acc12 gl_d in 
                                       res6 k13 q13 acc13 f4 in 
                                     shrink k14 q10 acc10 gl_t in 
                                   ev k11 q9 acc9 x4 in 
                        k9 q8 acc8 f3 in 
             let k5 q4 acc4 res1 =
               k4 q4 acc4 res1 in 
             f1 k5 q3 acc3 f2 in 
  let rec shrink k16 q15 acc15 t =
    let f5 = fun k17 q16 acc16 f ->
               let f6 = fun k18 q17 acc17 d ->
                          let x6 = () in 
                          let k22 q21 acc21 res10 =
                            let x7 = 0 in 
                            let x5 = res10 <= x7 in 
                            let k19 q18 acc18 res7 =
                              k18 q18 acc18 res7 in 
                            let k20 q19 acc19 res8 =
                              let x14 = 0 in 
                              k19 q19 acc19 x14 in 
                            let k21 q20 acc20 res9 =
                              let x11 = 1 in 
                              let x10 = () in 
                              let k23 q22 acc22 x9 =
                                let f7 = fun k25 q24 acc24 t_pm ->
                                           let k28 q27 acc27 res14 =
                                             let f8 = fun k29 q28 acc28 x ->
                                                        k29 q28 acc28 t_pm in 
                                             let k27 q26 acc26 res13 =
                                               let k26 q25 acc25 res12 =
                                                 k25 q25 acc25 res12 in 
                                               res13 k26 q26 acc26 d in 
                                             res14 k27 q27 acc27 f8 in 
                                           shrink k28 q24 acc24 t_pm in 
                                let x13 = () in 
                                let k30 q29 acc29 res15 =
                                  let x12 = res15 - d in  let k24 q23 acc23 res11 =
                                                            let x8 = x10 ; res11 in  k19 q23 acc23 x8 in 
                                                          f7 k24 q29 acc29 x12 in 
                                f k30 q22 acc22 x13 in 
                              ev k23 q20 acc20 x11 in 
                            if x5 then k20 q21 acc21 x5 else k21 q21 acc21 x5 in 
                          f k22 q17 acc17 x6 in 
               k17 q16 acc16 f6 in 
    k16 q15 acc15 f5 in 
  let k3 q2 acc2 res0 =
    let k31 q acc x15 =
      let x17 = prefgl_t / prefgl_d in  let x16 = acc = x17 in  assert(x16);x15 in 
    k31 q2 acc2 res0 in 
  f0 k3 q1 acc1 shrink