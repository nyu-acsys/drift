(* CPS conversion. Source Program: 

let rec listener npool pend n = 
  if n <= 0 then 0
  else
    if pend < npool then 
      begin
        ev 0;
        listener npool (pend+1) (n-1)
      end
    else if pend > 0 then
      begin
        ev 1;
        listener npool (pend-1) (n-1)
      end
    else
      begin
        ev 2;
        listener npool pend (n-1)
      end

let main (npool:int(*-:{v:Int | true}*)) (n:int(*-:{v:Int | true}*)) =
  if npool >= 0 then
    listener npool 0 n
  else 0

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

let main prefnpool prefn = 
  let ev = fun k0 q enq deq tick evx ->
             if ((q = 0) && (evx = 0)) then k0 q (enq + 1) deq tick () 
             else if ((q = 0) && (evx = 1)) then k0 q enq (deq + 1) tick () 
                  else if ((q = 0) && (evx = 2)) then k0 q enq deq (tick + 1) () 
                       else k0 1 enq deq tick () in 
  let q1 = 0 in 
  let enq1 = 0 in 
  let deq1 = 0 in 
  let tick1 = 0 in 
  let f0 = fun k4 q3 enq3 deq3 tick3 listener ->
             let f1 = fun k6 q5 enq5 deq5 tick5 _main ->
                        let k8 q7 enq7 deq7 tick7 res3 =
                          let k7 q6 enq6 deq6 tick6 res2 =
                            k6 q6 enq6 deq6 tick6 res2 in 
                          res3 k7 q7 enq7 deq7 tick7 prefn in 
                        _main k8 q5 enq5 deq5 tick5 prefnpool in 
             let f2 = fun k9 q8 enq8 deq8 tick8 npool ->
                        let f3 = fun k10 q9 enq9 deq9 tick9 n ->
                                   let x2 = 0 in 
                                   let x1 = npool >= x2 in 
                                   let k11 q10 enq10 deq10 tick10 res4 =
                                     k10 q10 enq10 deq10 tick10 res4 in 
                                   let k12 q11 enq11 deq11 tick11 res5 =
                                     let k16 q15 enq15 deq15 tick15 res9 =
                                       let x4 = 0 in 
                                       let k15 q14 enq14 deq14 tick14 res8 =
                                         let k14 q13 enq13 deq13 tick13 res7 =
                                           k11 q13 enq13 deq13 tick13 res7 in 
                                         res8 k14 q14 enq14 deq14 tick14 n in 
                                       res9 k15 q15 enq15 deq15 tick15 x4 in 
                                     listener k16 q11 enq11 deq11 tick11 npool in 
                                   let k13 q12 enq12 deq12 tick12 res6 =
                                     let x3 = 0 in 
                                     k11 q12 enq12 deq12 tick12 x3 in 
                                   if x1 then k12 q9 enq9 deq9 tick9 x1 else k13 q9 enq9 deq9 tick9 x1 in 
                        k9 q8 enq8 deq8 tick8 f3 in 
             let k5 q4 enq4 deq4 tick4 res1 =
               k4 q4 enq4 deq4 tick4 res1 in 
             f1 k5 q3 enq3 deq3 tick3 f2 in 
  let rec listener k17 q16 enq16 deq16 tick16 npool =
    let f4 = fun k18 q17 enq17 deq17 tick17 pend ->
               let f5 = fun k19 q18 enq18 deq18 tick18 n ->
                          let x6 = 0 in 
                          let x5 = n <= x6 in 
                          let k20 q19 enq19 deq19 tick19 res10 =
                            k19 q19 enq19 deq19 tick19 res10 in 
                          let k21 q20 enq20 deq20 tick20 res11 =
                            let x32 = 0 in 
                            k20 q20 enq20 deq20 tick20 x32 in 
                          let k22 q21 enq21 deq21 tick21 res12 =
                            let x7 = pend < npool in 
                            let k23 q22 enq22 deq22 tick22 res13 =
                              k20 q22 enq22 deq22 tick22 res13 in 
                            let k24 q23 enq23 deq23 tick23 res14 =
                              let x27 = 0 in 
                              let x26 = () in 
                              let k37 q36 enq36 deq36 tick36 x25 =
                                let k40 q39 enq39 deq39 tick39 res27 =
                                  let x29 = 1 in 
                                  let x28 = pend + x29 in 
                                  let k39 q38 enq38 deq38 tick38 res26 =
                                    let x31 = 1 in 
                                    let x30 = n - x31 in  let k38 q37 enq37 deq37 tick37 res25 =
                                                            let x24 = x26 ; res25 in  k23 q37 enq37 deq37 tick37 x24 in 
                                                          res26 k38 q38 enq38 deq38 tick38 x30 in 
                                  res27 k39 q39 enq39 deq39 tick39 x28 in 
                                listener k40 q36 enq36 deq36 tick36 npool in 
                              ev k37 q23 enq23 deq23 tick23 x27 in 
                            let k25 q24 enq24 deq24 tick24 res15 =
                              let x9 = 0 in 
                              let x8 = pend > x9 in 
                              let k26 q25 enq25 deq25 tick25 res16 =
                                k23 q25 enq25 deq25 tick25 res16 in 
                              let k27 q26 enq26 deq26 tick26 res17 =
                                let x19 = 1 in 
                                let x18 = () in 
                                let k33 q32 enq32 deq32 tick32 x17 =
                                  let k36 q35 enq35 deq35 tick35 res24 =
                                    let x21 = 1 in 
                                    let x20 = pend - x21 in 
                                    let k35 q34 enq34 deq34 tick34 res23 =
                                      let x23 = 1 in 
                                      let x22 = n - x23 in  let k34 q33 enq33 deq33 tick33 res22 =
                                                              let x16 = x18 ; res22 in  k26 q33 enq33 deq33 tick33 x16 in 
                                                            res23 k34 q34 enq34 deq34 tick34 x22 in 
                                    res24 k35 q35 enq35 deq35 tick35 x20 in 
                                  listener k36 q32 enq32 deq32 tick32 npool in 
                                ev k33 q26 enq26 deq26 tick26 x19 in 
                              let k28 q27 enq27 deq27 tick27 res18 =
                                let x13 = 2 in 
                                let x12 = () in 
                                let k29 q28 enq28 deq28 tick28 x11 =
                                  let k32 q31 enq31 deq31 tick31 res21 =
                                    let k31 q30 enq30 deq30 tick30 res20 =
                                      let x15 = 1 in 
                                      let x14 = n - x15 in  let k30 q29 enq29 deq29 tick29 res19 =
                                                              let x10 = x12 ; res19 in  k26 q29 enq29 deq29 tick29 x10 in 
                                                            res20 k30 q30 enq30 deq30 tick30 x14 in 
                                    res21 k31 q31 enq31 deq31 tick31 pend in 
                                  listener k32 q28 enq28 deq28 tick28 npool in 
                                ev k29 q27 enq27 deq27 tick27 x13 in 
                              if x8 then k27 q24 enq24 deq24 tick24 x8 else k28 q24 enq24 deq24 tick24 x8 in 
                            if x7 then k24 q21 enq21 deq21 tick21 x7 else k25 q21 enq21 deq21 tick21 x7 in 
                          if x5 then k21 q18 enq18 deq18 tick18 x5 else k22 q18 enq18 deq18 tick18 x5 in 
               k18 q17 enq17 deq17 tick17 f5 in 
    k17 q16 enq16 deq16 tick16 f4 in 
  let k3 q2 enq2 deq2 tick2 res0 =
    let k41 q enq deq tick x33 =
      let x37 = 0 in 
      let x36 = q = x37 in  let x39 = enq + prefl2 in  let x38 = tick = x39 in  let x35 = x36 && x38 in  let x41 = deq - prefl1 in  let x40 = tick = x41 in  let x34 = x35 && x40 in  assert(x34);x33 in 
    k41 q2 enq2 deq2 tick2 res0 in 
  f0 k3 q1 enq1 deq1 tick1 listener