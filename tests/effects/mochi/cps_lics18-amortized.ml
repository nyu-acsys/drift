(* CPS conversion. Source Program: 

let rec rev l =
  let rec aux l_aux l_acc =
    if l_aux = 0 then l_acc
    else begin ev 2; aux (l_aux-1) (l_acc+1) end
  in aux l 0

let rec dequeue (l1_deq, l2_deq) = 
  if (l1_deq > 0) then 
    begin ev 1; dequeue ((l1_deq-1), l2_deq) end
  else if l1_deq = 0 then
    (if l2_deq > 0 then dequeue ((rev l2_deq), 0)
          else 0) 
  else 0
  (* if (l1_deq = 0) then rev (l2_deq)
  else repeat_dequeue ((l1_deq -1), l2_deq) *)

let rec enqueue n_r (l1_eq, l2_eq) =
  if n_r = 0 then (l1_eq, l2_eq)
  else begin ev 0; enqueue (n_r-1) (l1_eq, l2_eq+1) end

let main (n:int(*-:{v:Int | v >= 0 }*)) (l1:int(*-:{v:Int | v >= 0 }*)) (l2:int(*-:{v:Int | v >= 0 }*)) =
  dequeue (enqueue n (l1, l2))


Property: 

QSet = [0;1];

(* [default state, error state] *)

delta = fun evx (q, (enq, deq, tick)) ->
    if (q=0 && evx = 0) then (q, (enq+1, deq, tick))
    else if (q=0 && evx = 1) then (q, (enq, deq+1, tick))
    else if (q=0 && evx = 2) then (q, (enq, deq, tick+1))
    else (1, (enq, deq, tick));

IniCfg = (0, (0,0,0));

assertFinal = fun (q, (enq, deq, tick)) -> q=0 && tick = enq+prefl2 && tick = deq-prefl1;
(* assertFinal = fun (q, (enq, deq, tick)) -> q=0 && tick = enq+prefl2; *)


*)

let main prefn prefl2 prefl1 = 
  let ev = fun k0 q enq deq tick evx ->
             if ((q = 0) && (evx = 0)) then k0 q (enq + 1) deq tick () 
             else if ((q = 0) && (evx = 1)) then k0 q enq (deq + 1) tick () 
                  else if ((q = 0) && (evx = 2)) then k0 q enq deq (tick + 1) () 
                       else k0 1 enq deq tick () in 
  let q1 = 0 in 
  let enq1 = 0 in 
  let deq1 = 0 in 
  let tick1 = 0 in 
  let f0 = fun k4 q3 enq3 deq3 tick3 rev ->
             let f1 = fun k6 q5 enq5 deq5 tick5 dequeue ->
                        let f2 = fun k8 q7 enq7 deq7 tick7 enqueue ->
                                   let f3 = fun k10 q9 enq9 deq9 tick9 _main ->
                                              let k13 q12 enq12 deq12 tick12 res6 =
                                                let k12 q11 enq11 deq11 tick11 res5 =
                                                  let k11 q10 enq10 deq10 tick10 res4 =
                                                    k10 q10 enq10 deq10 tick10 res4 in 
                                                  res5 k11 q11 enq11 deq11 tick11 prefl2 in 
                                                res6 k12 q12 enq12 deq12 tick12 prefl1 in 
                                              _main k13 q9 enq9 deq9 tick9 prefn in 
                                   let f4 = fun k14 q13 enq13 deq13 tick13 n ->
                                              let f5 = fun k15 q14 enq14 deq14 tick14 l1 ->
                                                         let f6 = fun k16 q15 enq15 deq15 tick15 l2 ->
                                                                    let k19 q18 enq18 deq18 tick18 res9 =
                                                                      let k18 q17 enq17 deq17 tick17 res8 =
                                                                        let k17 q16 enq16 deq16 tick16 res7 =
                                                                          k16 q16 enq16 deq16 tick16 res7 in 
                                                                        dequeue k17 q17 enq17 deq17 tick17 res8 in 
                                                                      res9 k18 q18 enq18 deq18 tick18 l1 l2 in 
                                                                    enqueue k19 q15 enq15 deq15 tick15 n in 
                                                         k15 q14 enq14 deq14 tick14 f6 in 
                                              k14 q13 enq13 deq13 tick13 f5 in 
                                   let k9 q8 enq8 deq8 tick8 res3 =
                                     k8 q8 enq8 deq8 tick8 res3 in 
                                   f3 k9 q7 enq7 deq7 tick7 f4 in 
                        let rec enqueue k20 q19 enq19 deq19 tick19 n_r =
                          let f7 = fun k21 q20 enq20 deq20 tick20 l1_eq l2_eq ->
                                     let x4 = 0 in 
                                     let x3 = n_r = x4 in 
                                     let k22 q21 enq21 deq21 tick21 res10 =
                                       k21 q21 enq21 deq21 tick21 res10 in 
                                     let k23 q22 enq22 deq22 tick22 res11 =
                                       k22 q22 enq22 deq22 tick22 l1_eq l2_eq in 
                                     let k24 q23 enq23 deq23 tick23 res12 =
                                       let x8 = 0 in 
                                       let x7 = () in 
                                       let k25 q24 enq24 deq24 tick24 x6 =
                                         let x10 = 1 in 
                                         let x9 = n_r - x10 in 
                                         let k27 q26 enq26 deq26 tick26 res14 =
                                           let x12 = 1 in 
                                           let x11 = l2_eq + x12 in 
                                           let k26 q25 enq25 deq25 tick25 res13 =
                                             let x5 = x7 ; res13 in  k22 q25 enq25 deq25 tick25 x5 in 
                                           res14 k26 q26 enq26 deq26 tick26 l1_eq x11 in 
                                         enqueue k27 q24 enq24 deq24 tick24 x9 in 
                                       ev k25 q23 enq23 deq23 tick23 x8 in 
                                     if x3 then k23 q20 enq20 deq20 tick20 x3 else k24 q20 enq20 deq20 tick20 x3 in 
                          k20 q19 enq19 deq19 tick19 f7 in 
                        let k7 q6 enq6 deq6 tick6 res2 =
                          k6 q6 enq6 deq6 tick6 res2 in 
                        f2 k7 q5 enq5 deq5 tick5 enqueue in 
             let rec dequeue k30 q29 enq29 deq29 tick29 l1_deq l2_deq =
               let x14 = 0 in 
               let x13 = l1_deq > x14 in 
               let k31 q30 enq30 deq30 tick30 res17 =
                 k30 q30 enq30 deq30 tick30 res17 in 
               let k32 q31 enq31 deq31 tick31 res18 =
                 let x25 = 1 in 
                 let x24 = () in 
                 let k42 q41 enq41 deq41 tick41 x23 =
                   let x27 = 1 in 
                   let x26 = l1_deq - x27 in  let k43 q42 enq42 deq42 tick42 res28 =
                                                let x22 = x24 ; res28 in  k31 q42 enq42 deq42 tick42 x22 in 
                                              dequeue k43 q41 enq41 deq41 tick41 x26 l2_deq in 
                 ev k42 q31 enq31 deq31 tick31 x25 in 
               let k33 q32 enq32 deq32 tick32 res19 =
                 let x16 = 0 in 
                 let x15 = l1_deq = x16 in 
                 let k34 q33 enq33 deq33 tick33 res20 =
                   k31 q33 enq33 deq33 tick33 res20 in 
                 let k35 q34 enq34 deq34 tick34 res21 =
                   let x19 = 0 in 
                   let x18 = l2_deq > x19 in 
                   let k37 q36 enq36 deq36 tick36 res23 =
                     k34 q36 enq36 deq36 tick36 res23 in 
                   let k38 q37 enq37 deq37 tick37 res24 =
                     let k41 q40 enq40 deq40 tick40 res27 =
                       let x21 = 0 in 
                       let k40 q39 enq39 deq39 tick39 res26 =
                         k37 q39 enq39 deq39 tick39 res26 in 
                       dequeue k40 q40 enq40 deq40 tick40 res27 x21 in 
                     rev k41 q37 enq37 deq37 tick37 l2_deq in 
                   let k39 q38 enq38 deq38 tick38 res25 =
                     let x20 = 0 in 
                     k37 q38 enq38 deq38 tick38 x20 in 
                   if x18 then k38 q34 enq34 deq34 tick34 x18 else k39 q34 enq34 deq34 tick34 x18 in 
                 let k36 q35 enq35 deq35 tick35 res22 =
                   let x17 = 0 in 
                   k34 q35 enq35 deq35 tick35 x17 in 
                 if x15 then k35 q32 enq32 deq32 tick32 x15 else k36 q32 enq32 deq32 tick32 x15 in 
               if x13 then k32 q29 enq29 deq29 tick29 x13 else k33 q29 enq29 deq29 tick29 x13 in 
             let k5 q4 enq4 deq4 tick4 res1 =
               k4 q4 enq4 deq4 tick4 res1 in 
             f1 k5 q3 enq3 deq3 tick3 dequeue in 
  let f8 = fun k44 q43 enq43 deq43 tick43 l ->
             let f9 = fun k46 q45 enq45 deq45 tick45 aux ->
                        let k48 q47 enq47 deq47 tick47 res31 =
                          let x28 = 0 in 
                          let k47 q46 enq46 deq46 tick46 res30 =
                            k46 q46 enq46 deq46 tick46 res30 in 
                          res31 k47 q47 enq47 deq47 tick47 x28 in 
                        aux k48 q45 enq45 deq45 tick45 l in 
             let rec aux k49 q48 enq48 deq48 tick48 l_aux =
               let f10 = fun k50 q49 enq49 deq49 tick49 l_acc ->
                           let x30 = 0 in 
                           let x29 = l_aux = x30 in 
                           let k51 q50 enq50 deq50 tick50 res32 =
                             k50 q50 enq50 deq50 tick50 res32 in 
                           let k52 q51 enq51 deq51 tick51 res33 =
                             k51 q51 enq51 deq51 tick51 l_acc in 
                           let k53 q52 enq52 deq52 tick52 res34 =
                             let x34 = 2 in 
                             let x33 = () in 
                             let k54 q53 enq53 deq53 tick53 x32 =
                               let x36 = 1 in 
                               let x35 = l_aux - x36 in 
                               let k56 q55 enq55 deq55 tick55 res36 =
                                 let x38 = 1 in 
                                 let x37 = l_acc + x38 in  let k55 q54 enq54 deq54 tick54 res35 =
                                                             let x31 = x33 ; res35 in  k51 q54 enq54 deq54 tick54 x31 in 
                                                           res36 k55 q55 enq55 deq55 tick55 x37 in 
                               aux k56 q53 enq53 deq53 tick53 x35 in 
                             ev k54 q52 enq52 deq52 tick52 x34 in 
                           if x29 then k52 q49 enq49 deq49 tick49 x29 else k53 q49 enq49 deq49 tick49 x29 in 
               k49 q48 enq48 deq48 tick48 f10 in 
             let k45 q44 enq44 deq44 tick44 res29 =
               k44 q44 enq44 deq44 tick44 res29 in 
             f9 k45 q43 enq43 deq43 tick43 aux in 
  let k3 q2 enq2 deq2 tick2 res0 =
    let k57 q enq deq tick x39 =
      let x43 = 0 in 
      let x42 = q = x43 in  let x45 = enq + prefl2 in  let x44 = tick = x45 in  let x41 = x42 && x44 in  let x47 = deq - prefl1 in  let x46 = tick = x47 in  let x40 = x41 && x46 in  assert(x40);x39 in 
    k57 q2 enq2 deq2 tick2 res0 in 
  f0 k3 q1 enq1 deq1 tick1 f8