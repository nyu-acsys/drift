(* CPS conversion. Source Program: 

let rec f x pos neg =
  if x > 0 then
    ev pos
  else if x < 0 then  
    ev neg
  else 
    ();

  if x > 0 then
    f (x-1) pos neg
  else if x < 0 then 
    f (x+1) pos neg
  else
    0
  
let main (v:int(*-:{v:Int | true}*)) (p:int(*-:{v:Int | true}*)) (n:int(*-:{v:Int | true}*)) =
  if p > 0 && n < 0 then
    f v p n
  else 0 


Property: 

(* Disjunctive *)

QSet   = [0;1;2;3]; 

delta  = fun evx (q, acc) -> 
       	     if ((q = 0) || (q = 1)) && (evx > 0) then (1,acc)
	     else if (q = 1) && (evx < 0) then (3, acc)
       	     else if ((q = 0) || (q = 2)) && (evx < 0) then (2,acc)
	     else if (q = 2) && (evx > 0) then (3, acc)
	     else (3, acc);

IniCfg = (0, 0);

(* if event is pos, x must  be even ; *)
assert = fun (q, acc) -> (q < 3);



*)

let main prefv prefp prefn = 
  let ev = fun k0 q acc evx ->
             if (((q = 0) || (q = 1)) && (evx > 0)) then k0 1 acc () 
             else if ((q = 1) && (evx < 0)) then k0 3 acc () 
                  else if (((q = 0) || (q = 2)) && (evx < 0)) then k0 2 acc () 
                       else if ((q = 2) && (evx > 0)) then k0 3 acc () 
                            else k0 3 acc () in 
  let ev_assert = fun k1 q0 acc0 x0 ->
                    let k42 q acc x27 =
                      let x29 = 3 in 
                      let x28 = q < x29 in  let x26 = () in 
                                            assert(x28);k1 q acc x26 in 
                    ev k42 q0 acc0 x0 in 
  let q1 = 0 in 
  let acc1 = 0 in 
  let f0 = fun k4 q3 acc3 f ->
             let f1 = fun k6 q5 acc5 _main ->
                        let k9 q8 acc8 res4 =
                          let k8 q7 acc7 res3 =
                            let k7 q6 acc6 res2 =
                              k6 q6 acc6 res2 in 
                            res3 k7 q7 acc7 prefn in 
                          res4 k8 q8 acc8 prefp in 
                        _main k9 q5 acc5 prefv in 
             let f2 = fun k10 q9 acc9 v ->
                        let f3 = fun k11 q10 acc10 p ->
                                   let f4 = fun k12 q11 acc11 n ->
                                              let x3 = 0 in 
                                              let x2 = p > x3 in 
                                              let x5 = 0 in 
                                              let x4 = n < x5 in 
                                              let x1 = x2 && x4 in 
                                              let k13 q12 acc12 res5 =
                                                k12 q12 acc12 res5 in 
                                              let k14 q13 acc13 res6 =
                                                let k18 q17 acc17 res10 =
                                                  let k17 q16 acc16 res9 =
                                                    let k16 q15 acc15 res8 =
                                                      k13 q15 acc15 res8 in 
                                                    res9 k16 q16 acc16 n in 
                                                  res10 k17 q17 acc17 p in 
                                                f k18 q13 acc13 v in 
                                              let k15 q14 acc14 res7 =
                                                let x6 = 0 in 
                                                k13 q14 acc14 x6 in 
                                              if x1 then k14 q11 acc11 x1 else k15 q11 acc11 x1 in 
                                   k11 q10 acc10 f4 in 
                        k10 q9 acc9 f3 in 
             let k5 q4 acc4 res1 =
               k4 q4 acc4 res1 in 
             f1 k5 q3 acc3 f2 in 
  let rec f k19 q18 acc18 x =
    let f5 = fun k20 q19 acc19 pos ->
               let f6 = fun k21 q20 acc20 neg ->
                          let x9 = 0 in 
                          let x8 = x > x9 in 
                          let k22 q21 acc21 res11 =
                            let x18 = 0 in 
                            let x17 = x > x18 in 
                            let k30 q29 acc29 res17 =
                              let x7 = res11 ; res17 in  k21 q29 acc29 x7 in 
                            let k31 q30 acc30 res18 =
                              let x25 = 1 in 
                              let x24 = x - x25 in 
                              let k41 q40 acc40 res28 =
                                let k40 q39 acc39 res27 =
                                  let k39 q38 acc38 res26 =
                                    k30 q38 acc38 res26 in 
                                  res27 k39 q39 acc39 neg in 
                                res28 k40 q40 acc40 pos in 
                              f k41 q30 acc30 x24 in 
                            let k32 q31 acc31 res19 =
                              let x20 = 0 in 
                              let x19 = x < x20 in 
                              let k33 q32 acc32 res20 =
                                k30 q32 acc32 res20 in 
                              let k34 q33 acc33 res21 =
                                let x23 = 1 in 
                                let x22 = x + x23 in 
                                let k38 q37 acc37 res25 =
                                  let k37 q36 acc36 res24 =
                                    let k36 q35 acc35 res23 =
                                      k33 q35 acc35 res23 in 
                                    res24 k36 q36 acc36 neg in 
                                  res25 k37 q37 acc37 pos in 
                                f k38 q33 acc33 x22 in 
                              let k35 q34 acc34 res22 =
                                let x21 = 0 in 
                                k33 q34 acc34 x21 in 
                              if x19 then k34 q31 acc31 x19 else k35 q31 acc31 x19 in 
                            if x17 then k31 q21 acc21 x17 else k32 q21 acc21 x17 in 
                          let k23 q22 acc22 res12 =
                            let x16 = () in 
                            let k29 q28 acc28 x15 =
                              k22 q28 acc28 x16 in 
                            ev_assert k29 q22 acc22 pos in 
                          let k24 q23 acc23 res13 =
                            let x11 = 0 in 
                            let x10 = x < x11 in 
                            let k25 q24 acc24 res14 =
                              k22 q24 acc24 res14 in 
                            let k26 q25 acc25 res15 =
                              let x14 = () in 
                              let k28 q27 acc27 x13 =
                                k25 q27 acc27 x14 in 
                              ev_assert k28 q25 acc25 neg in 
                            let k27 q26 acc26 res16 =
                              let x12 = () in 
                              k25 q26 acc26 x12 in 
                            if x10 then k26 q23 acc23 x10 else k27 q23 acc23 x10 in 
                          if x8 then k23 q20 acc20 x8 else k24 q20 acc20 x8 in 
               k20 q19 acc19 f6 in 
    k19 q18 acc18 f5 in 
  let k3 q2 acc2 res0 =
    res0 in 
  f0 k3 q1 acc1 f