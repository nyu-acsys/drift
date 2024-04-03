(* CPS conversion. Source Program: 

let rev l = 
  let rec aux l_aux l_acc = 
    if l_aux = 0 then l_acc
    else begin ev 2; aux (l_aux-1) l_acc+1 end
  in aux l 0


(* let is_empty (l1_e, l2_e) = l1_e = 0 && l2_e = 0

let rec dequeue (l1_dq, l2_dq) = 
  if l1_dq = 0 then dequeue (rev l2_dq, 0)
  else begin ev 1; (l1_dq-1, l2_dq) end

let rec enqueue n_r (l1_eq, l2_eq) =
  if n_r = 0 then (l1_eq, l2_eq)
  else begin ev 0; enqueue (n_r-1) (l1_eq, l2_eq+1) end

let rec repeat_dequeue (l1_dqr, l2_dqr) =
  if is_empty (l1_dqr, l2_dqr) then 0
  else repeat_dequeue (dequeue (l1_dqr, l2_dqr))

let main (n:int(*-:{v:Int | true}*)) (l1:int(*-:{v:Int | true}*)) (l2:int(*-:{v:Int | true}*)) =
  if (l1>=0 && l2>=0) then
    let (l1_1, l2_1) =
      if n>=0 then enqueue n (l1, l2)
      else (l1, l2)
    in 
    repeat_dequeue (l1_1, l2_1)
  else
    0 *)

let is_empty l1_e l2_e = l1_e = 0 && l2_e = 0

let rec dequeue l1_dq l2_dq f_dq = 
  if l1_dq = 0 then dequeue (rev l2_dq) 0 f_dq
  else begin ev 1; f_dq (l1_dq-1) l2_dq end

let rec enqueue n_r l1_eq l2_eq f_eq =
  if n_r = 0 then f_eq l1_eq l2_eq
  else begin ev 0; enqueue (n_r-1) l1_eq (l2_eq+1) f_eq end

let rec repeat_dequeue l1_dqr l2_dqr =
  if is_empty l1_dqr l2_dqr then 0
  else dequeue l1_dqr l2_dqr (fun a1 -> fun b1 -> repeat_dequeue a1 b1)

let main (n:int(*-:{v:Int | true}*)) (l1:int(*-:{v:Int | true}*)) (l2:int(*-:{v:Int | true}*)) =
  if (l1>=0 && l2>=0) then
    if n>=0 then 
      enqueue n l1 l2 (fun a2 -> fun b2 -> repeat_dequeue a2 b2)
    else repeat_dequeue l1 l2
  else
    0

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
             let f1 = fun k6 q5 enq5 deq5 tick5 is_empty ->
                        let f2 = fun k8 q7 enq7 deq7 tick7 dequeue ->
                                   let f3 = fun k10 q9 enq9 deq9 tick9 enqueue ->
                                              let f4 = fun k12 q11 enq11 deq11 tick11 repeat_dequeue ->
                                                         let f5 = fun k14 q13 enq13 deq13 tick13 _main ->
                                                                    let k17 q16 enq16 deq16 tick16 res8 =
                                                                      let k16 q15 enq15 deq15 tick15 res7 =
                                                                        let k15 q14 enq14 deq14 tick14 res6 =
                                                                          k14 q14 enq14 deq14 tick14 res6 in 
                                                                        res7 k15 q15 enq15 deq15 tick15 prefl2 in 
                                                                      res8 k16 q16 enq16 deq16 tick16 prefl1 in 
                                                                    _main k17 q13 enq13 deq13 tick13 prefn in 
                                                         let f6 = fun k18 q17 enq17 deq17 tick17 n ->
                                                                    let f7 = fun k19 q18 enq18 deq18 tick18 l1 ->
                                                                               let f8 = fun k20 q19 enq19 deq19 tick19 l2 ->
                                                                                          let x3 = 0 in 
                                                                                          let x2 = l1 >= x3 in 
                                                                                          let x5 = 0 in 
                                                                                          let x4 = l2 >= x5 in 
                                                                                          let x1 = x2 && x4 in 
                                                                                          let k21 q20 enq20 deq20 tick20 res9 =
                                                                                            k20 q20 enq20 deq20 tick20 res9 in 
                                                                                          let k22 q21 enq21 deq21 tick21 res10 =
                                                                                            let x8 = 0 in 
                                                                                            let x7 = n >= x8 in 
                                                                                            let k24 q23 enq23 deq23 tick23 res12 =
                                                                                              k21 q23 enq23 deq23 tick23 res12 in 
                                                                                            let k25 q24 enq24 deq24 tick24 res13 =
                                                                                              let k32 q31 enq31 deq31 tick31 res20 =
                                                                                                let k31 q30 enq30 deq30 tick30 res19 =
                                                                                                  let k30 q29 enq29 deq29 tick29 res18 =
                                                                                                    let f9 = fun k33 q32 enq32 deq32 tick32 a2 ->
                                                                                                               let f10 = fun k34 q33 enq33 deq33 tick33 b2 ->
                                                                                                                           let k36 q35 enq35 deq35 tick35 res22 =
                                                                                                                             let k35 q34 enq34 deq34 tick34 res21 =
                                                                                                                               k34 q34 enq34 deq34 tick34 res21 in 
                                                                                                                             res22 k35 q35 enq35 deq35 tick35 b2 in 
                                                                                                                           repeat_dequeue k36 q33 enq33 deq33 tick33 a2 in 
                                                                                                               k33 q32 enq32 deq32 tick32 f10 in 
                                                                                                    let k29 q28 enq28 deq28 tick28 res17 =
                                                                                                      k24 q28 enq28 deq28 tick28 res17 in 
                                                                                                    res18 k29 q29 enq29 deq29 tick29 f9 in 
                                                                                                  res19 k30 q30 enq30 deq30 tick30 l2 in 
                                                                                                res20 k31 q31 enq31 deq31 tick31 l1 in 
                                                                                              enqueue k32 q24 enq24 deq24 tick24 n in 
                                                                                            let k26 q25 enq25 deq25 tick25 res14 =
                                                                                              let k28 q27 enq27 deq27 tick27 res16 =
                                                                                                let k27 q26 enq26 deq26 tick26 res15 =
                                                                                                  k24 q26 enq26 deq26 tick26 res15 in 
                                                                                                res16 k27 q27 enq27 deq27 tick27 l2 in 
                                                                                              repeat_dequeue k28 q25 enq25 deq25 tick25 l1 in 
                                                                                            if x7 then k25 q21 enq21 deq21 tick21 x7 else k26 q21 enq21 deq21 tick21 x7 in 
                                                                                          let k23 q22 enq22 deq22 tick22 res11 =
                                                                                            let x6 = 0 in 
                                                                                            k21 q22 enq22 deq22 tick22 x6 in 
                                                                                          if x1 then k22 q19 enq19 deq19 tick19 x1 else k23 q19 enq19 deq19 tick19 x1 in 
                                                                               k19 q18 enq18 deq18 tick18 f8 in 
                                                                    k18 q17 enq17 deq17 tick17 f7 in 
                                                         let k13 q12 enq12 deq12 tick12 res5 =
                                                           k12 q12 enq12 deq12 tick12 res5 in 
                                                         f5 k13 q11 enq11 deq11 tick11 f6 in 
                                              let rec repeat_dequeue k37 q36 enq36 deq36 tick36 l1_dqr =
                                                let f11 = fun k38 q37 enq37 deq37 tick37 l2_dqr ->
                                                            let k43 q42 enq42 deq42 tick42 res27 =
                                                              let k42 q41 enq41 deq41 tick41 res26 =
                                                                let k39 q38 enq38 deq38 tick38 res23 =
                                                                  k38 q38 enq38 deq38 tick38 res23 in 
                                                                let k40 q39 enq39 deq39 tick39 res24 =
                                                                  let x9 = 0 in 
                                                                  k39 q39 enq39 deq39 tick39 x9 in 
                                                                let k41 q40 enq40 deq40 tick40 res25 =
                                                                  let k46 q45 enq45 deq45 tick45 res30 =
                                                                    let k45 q44 enq44 deq44 tick44 res29 =
                                                                      let f12 = fun k47 q46 enq46 deq46 tick46 a1 ->
                                                                                  let f13 = fun k48 q47 enq47 deq47 tick47 b1 ->
                                                                                              let k50 q49 enq49 deq49 tick49 res32 =
                                                                                                let k49 q48 enq48 deq48 tick48 res31 =
                                                                                                  k48 q48 enq48 deq48 tick48 res31 in 
                                                                                                res32 k49 q49 enq49 deq49 tick49 b1 in 
                                                                                              repeat_dequeue k50 q47 enq47 deq47 tick47 a1 in 
                                                                                  k47 q46 enq46 deq46 tick46 f13 in 
                                                                      let k44 q43 enq43 deq43 tick43 res28 =
                                                                        k39 q43 enq43 deq43 tick43 res28 in 
                                                                      res29 k44 q44 enq44 deq44 tick44 f12 in 
                                                                    res30 k45 q45 enq45 deq45 tick45 l2_dqr in 
                                                                  dequeue k46 q40 enq40 deq40 tick40 l1_dqr in 
                                                                if res26 then k40 q41 enq41 deq41 tick41 res26 else k41 q41 enq41 deq41 tick41 res26 in 
                                                              res27 k42 q42 enq42 deq42 tick42 l2_dqr in 
                                                            is_empty k43 q37 enq37 deq37 tick37 l1_dqr in 
                                                k37 q36 enq36 deq36 tick36 f11 in 
                                              let k11 q10 enq10 deq10 tick10 res4 =
                                                k10 q10 enq10 deq10 tick10 res4 in 
                                              f4 k11 q9 enq9 deq9 tick9 repeat_dequeue in 
                                   let rec enqueue k51 q50 enq50 deq50 tick50 n_r =
                                     let f14 = fun k52 q51 enq51 deq51 tick51 l1_eq ->
                                                 let f15 = fun k53 q52 enq52 deq52 tick52 l2_eq ->
                                                             let f16 = fun k54 q53 enq53 deq53 tick53 f_eq ->
                                                                         let x11 = 0 in 
                                                                         let x10 = n_r = x11 in 
                                                                         let k55 q54 enq54 deq54 tick54 res33 =
                                                                           k54 q54 enq54 deq54 tick54 res33 in 
                                                                         let k56 q55 enq55 deq55 tick55 res34 =
                                                                           let k64 q63 enq63 deq63 tick63 res41 =
                                                                             let k63 q62 enq62 deq62 tick62 res40 =
                                                                               k55 q62 enq62 deq62 tick62 res40 in 
                                                                             res41 k63 q63 enq63 deq63 tick63 l2_eq in 
                                                                           f_eq k64 q55 enq55 deq55 tick55 l1_eq in 
                                                                         let k57 q56 enq56 deq56 tick56 res35 =
                                                                           let x15 = 0 in 
                                                                           let x14 = () in 
                                                                           let k58 q57 enq57 deq57 tick57 x13 =
                                                                             let x17 = 1 in 
                                                                             let x16 = n_r - x17 in 
                                                                             let k62 q61 enq61 deq61 tick61 res39 =
                                                                               let k61 q60 enq60 deq60 tick60 res38 =
                                                                                 let x19 = 1 in 
                                                                                 let x18 = l2_eq + x19 in 
                                                                                 let k60 q59 enq59 deq59 tick59 res37 =
                                                                                   let k59 q58 enq58 deq58 tick58 res36 =
                                                                                     let x12 = x14 ; res36 in  k55 q58 enq58 deq58 tick58 x12 in 
                                                                                   res37 k59 q59 enq59 deq59 tick59 f_eq in 
                                                                                 res38 k60 q60 enq60 deq60 tick60 x18 in 
                                                                               res39 k61 q61 enq61 deq61 tick61 l1_eq in 
                                                                             enqueue k62 q57 enq57 deq57 tick57 x16 in 
                                                                           ev k58 q56 enq56 deq56 tick56 x15 in 
                                                                         if x10 then k56 q53 enq53 deq53 tick53 x10 else k57 q53 enq53 deq53 tick53 x10 in 
                                                             k53 q52 enq52 deq52 tick52 f16 in 
                                                 k52 q51 enq51 deq51 tick51 f15 in 
                                     k51 q50 enq50 deq50 tick50 f14 in 
                                   let k9 q8 enq8 deq8 tick8 res3 =
                                     k8 q8 enq8 deq8 tick8 res3 in 
                                   f3 k9 q7 enq7 deq7 tick7 enqueue in 
                        let rec dequeue k65 q64 enq64 deq64 tick64 l1_dq =
                          let f17 = fun k66 q65 enq65 deq65 tick65 l2_dq ->
                                      let f18 = fun k67 q66 enq66 deq66 tick66 f_dq ->
                                                  let x21 = 0 in 
                                                  let x20 = l1_dq = x21 in 
                                                  let k68 q67 enq67 deq67 tick67 res42 =
                                                    k67 q67 enq67 deq67 tick67 res42 in 
                                                  let k69 q68 enq68 deq68 tick68 res43 =
                                                    let k77 q76 enq76 deq76 tick76 res50 =
                                                      let k76 q75 enq75 deq75 tick75 res49 =
                                                        let x28 = 0 in 
                                                        let k75 q74 enq74 deq74 tick74 res48 =
                                                          let k74 q73 enq73 deq73 tick73 res47 =
                                                            k68 q73 enq73 deq73 tick73 res47 in 
                                                          res48 k74 q74 enq74 deq74 tick74 f_dq in 
                                                        res49 k75 q75 enq75 deq75 tick75 x28 in 
                                                      dequeue k76 q76 enq76 deq76 tick76 res50 in 
                                                    rev k77 q68 enq68 deq68 tick68 l2_dq in 
                                                  let k70 q69 enq69 deq69 tick69 res44 =
                                                    let x25 = 1 in 
                                                    let x24 = () in 
                                                    let k71 q70 enq70 deq70 tick70 x23 =
                                                      let x27 = 1 in 
                                                      let x26 = l1_dq - x27 in 
                                                      let k73 q72 enq72 deq72 tick72 res46 =
                                                        let k72 q71 enq71 deq71 tick71 res45 =
                                                          let x22 = x24 ; res45 in  k68 q71 enq71 deq71 tick71 x22 in 
                                                        res46 k72 q72 enq72 deq72 tick72 l2_dq in 
                                                      f_dq k73 q70 enq70 deq70 tick70 x26 in 
                                                    ev k71 q69 enq69 deq69 tick69 x25 in 
                                                  if x20 then k69 q66 enq66 deq66 tick66 x20 else k70 q66 enq66 deq66 tick66 x20 in 
                                      k66 q65 enq65 deq65 tick65 f18 in 
                          k65 q64 enq64 deq64 tick64 f17 in 
                        let k7 q6 enq6 deq6 tick6 res2 =
                          k6 q6 enq6 deq6 tick6 res2 in 
                        f2 k7 q5 enq5 deq5 tick5 dequeue in 
             let f19 = fun k78 q77 enq77 deq77 tick77 l1_e ->
                         let f20 = fun k79 q78 enq78 deq78 tick78 l2_e ->
                                     let x31 = 0 in 
                                     let x30 = l1_e = x31 in  let x33 = 0 in 
                                                              let x32 = l2_e = x33 in  let x29 = x30 && x32 in  k79 q78 enq78 deq78 tick78 x29 in 
                         k78 q77 enq77 deq77 tick77 f20 in 
             let k5 q4 enq4 deq4 tick4 res1 =
               k4 q4 enq4 deq4 tick4 res1 in 
             f1 k5 q3 enq3 deq3 tick3 f19 in 
  let f21 = fun k80 q79 enq79 deq79 tick79 l ->
              let f22 = fun k82 q81 enq81 deq81 tick81 aux ->
                          let k84 q83 enq83 deq83 tick83 res53 =
                            let x34 = 0 in 
                            let k83 q82 enq82 deq82 tick82 res52 =
                              k82 q82 enq82 deq82 tick82 res52 in 
                            res53 k83 q83 enq83 deq83 tick83 x34 in 
                          aux k84 q81 enq81 deq81 tick81 l in 
              let rec aux k85 q84 enq84 deq84 tick84 l_aux =
                let f23 = fun k86 q85 enq85 deq85 tick85 l_acc ->
                            let x36 = 0 in 
                            let x35 = l_aux = x36 in 
                            let k87 q86 enq86 deq86 tick86 res54 =
                              k86 q86 enq86 deq86 tick86 res54 in 
                            let k88 q87 enq87 deq87 tick87 res55 =
                              k87 q87 enq87 deq87 tick87 l_acc in 
                            let k89 q88 enq88 deq88 tick88 res56 =
                              let x40 = 2 in 
                              let x39 = () in 
                              let k90 q89 enq89 deq89 tick89 x38 =
                                let x43 = 1 in 
                                let x42 = l_aux - x43 in 
                                let k92 q91 enq91 deq91 tick91 res58 =
                                  let k91 q90 enq90 deq90 tick90 res57 =
                                    let x44 = 1 in 
                                    let x41 = res57 + x44 in  let x37 = x39 ; x41 in  k87 q90 enq90 deq90 tick90 x37 in 
                                  res58 k91 q91 enq91 deq91 tick91 l_acc in 
                                aux k92 q89 enq89 deq89 tick89 x42 in 
                              ev k90 q88 enq88 deq88 tick88 x40 in 
                            if x35 then k88 q85 enq85 deq85 tick85 x35 else k89 q85 enq85 deq85 tick85 x35 in 
                k85 q84 enq84 deq84 tick84 f23 in 
              let k81 q80 enq80 deq80 tick80 res51 =
                k80 q80 enq80 deq80 tick80 res51 in 
              f22 k81 q79 enq79 deq79 tick79 aux in 
  let k3 q2 enq2 deq2 tick2 res0 =
    let k93 q enq deq tick x45 =
      let x49 = 0 in 
      let x48 = q = x49 in  let x51 = enq + prefl2 in  let x50 = tick = x51 in  let x47 = x48 && x50 in  let x53 = deq - prefl1 in  let x52 = tick = x53 in  let x46 = x47 && x52 in  assert(x46);x45 in 
    k93 q2 enq2 deq2 tick2 res0 in 
  f0 k3 q1 enq1 deq1 tick1 f21