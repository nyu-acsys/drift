(* CPS conversion. Source Program: 


let rec compute vv bound1 inc = 
    ev vv;
    if vv = bound1 then 0 else compute (inc vv) bound1 inc

let main (v:int(*-:{v:Int | true}*)) (w:int(*-:{v:Int | true}*)) (m:int(*-:{v:Int | true}*)) =
  let f = (fun t -> if v >= 0 then t-1 else t+1) in
  if (v>=0 && v <= 100000) then
    let bound = -v in
    compute v bound f
  else if (v<0 && v >= (-100000)) then
    let bound = -v in
    compute v bound f
  else
    0


Property: 

(* Max == -1 * Min *)
 
QSet   = [0;1]; 

delta  = fun evx (q, (max,min)) -> 
   if evx < min && evx > max then (1, (evx, evx))
   else if evx < min then (1, (max,evx)) 
   else if evx > max then (1, (evx,min)) 
   else (1, (max,min));

IniCfg = (0, (0,0));

assertFinal = fun (q, (max,min)) -> max + min = 0;


*)

let main prefw prefv prefm = 
  let ev = fun k0 q max min evx ->
             if ((evx < min) && (evx > max)) then k0 1 evx evx () 
             else if (evx < min) then k0 1 max evx () 
                  else if (evx > max) then k0 1 evx min () 
                       else k0 1 max min () in 
  let q1 = 0 in 
  let max1 = -100000 in 
  let min1 = 100000 in 
  let f0 = fun k4 q3 max3 min3 compute ->
             let f1 = fun k6 q5 max5 min5 _main ->
                        let k9 q8 max8 min8 res4 =
                          let k8 q7 max7 min7 res3 =
                            let k7 q6 max6 min6 res2 =
                              k6 q6 max6 min6 res2 in 
                            res3 k7 q7 max7 min7 prefm in 
                          res4 k8 q8 max8 min8 prefw in 
                        _main k9 q5 max5 min5 prefv in 
             let f2 = fun k10 q9 max9 min9 v ->
                        let f3 = fun k11 q10 max10 min10 w ->
                                   let f4 = fun k12 q11 max11 min11 m ->
                                              let f5 = fun k14 q13 max13 min13 f ->
                                                         let x3 = 0 in 
                                                         let x2 = v >= x3 in 
                                                         let x5 = 100000 in 
                                                         let x4 = v <= x5 in 
                                                         let x1 = x2 && x4 in 
                                                         let k15 q14 max14 min14 res6 =
                                                           k14 q14 max14 min14 res6 in 
                                                         let k16 q15 max15 min15 res7 =
                                                           let f7 = fun k27 q26 max26 min26 bound ->
                                                                      let k30 q29 max29 min29 res19 =
                                                                        let k29 q28 max28 min28 res18 =
                                                                          let k28 q27 max27 min27 res17 =
                                                                            k27 q27 max27 min27 res17 in 
                                                                          res18 k28 q28 max28 min28 f in 
                                                                        res19 k29 q29 max29 min29 bound in 
                                                                      compute k30 q26 max26 min26 v in 
                                                           let x13 = - v in  let k26 q25 max25 min25 res16 =
                                                                               k15 q25 max25 min25 res16 in 
                                                                             f7 k26 q15 max15 min15 x13 in 
                                                         let k17 q16 max16 min16 res8 =
                                                           let x8 = 0 in 
                                                           let x7 = v < x8 in 
                                                           let x10 = -100000 in 
                                                           let x9 = v >= x10 in 
                                                           let x6 = x7 && x9 in 
                                                           let k18 q17 max17 min17 res9 =
                                                             k15 q17 max17 min17 res9 in 
                                                           let k19 q18 max18 min18 res10 =
                                                             let f6 = fun k22 q21 max21 min21 bound ->
                                                                        let k25 q24 max24 min24 res15 =
                                                                          let k24 q23 max23 min23 res14 =
                                                                            let k23 q22 max22 min22 res13 =
                                                                              k22 q22 max22 min22 res13 in 
                                                                            res14 k23 q23 max23 min23 f in 
                                                                          res15 k24 q24 max24 min24 bound in 
                                                                        compute k25 q21 max21 min21 v in 
                                                             let x12 = - v in  let k21 q20 max20 min20 res12 =
                                                                                 k18 q20 max20 min20 res12 in 
                                                                               f6 k21 q18 max18 min18 x12 in 
                                                           let k20 q19 max19 min19 res11 =
                                                             let x11 = 0 in 
                                                             k18 q19 max19 min19 x11 in 
                                                           if x6 then k19 q16 max16 min16 x6 else k20 q16 max16 min16 x6 in 
                                                         if x1 then k16 q13 max13 min13 x1 else k17 q13 max13 min13 x1 in 
                                              let f8 = fun k31 q30 max30 min30 t ->
                                                         let x15 = 0 in 
                                                         let x14 = v >= x15 in 
                                                         let k32 q31 max31 min31 res20 =
                                                           k31 q31 max31 min31 res20 in 
                                                         let k33 q32 max32 min32 res21 =
                                                           let x19 = 1 in 
                                                           let x18 = t - x19 in  k32 q32 max32 min32 x18 in 
                                                         let k34 q33 max33 min33 res22 =
                                                           let x17 = 1 in 
                                                           let x16 = t + x17 in  k32 q33 max33 min33 x16 in 
                                                         if x14 then k33 q30 max30 min30 x14 else k34 q30 max30 min30 x14 in 
                                              let k13 q12 max12 min12 res5 =
                                                k12 q12 max12 min12 res5 in 
                                              f5 k13 q11 max11 min11 f8 in 
                                   k11 q10 max10 min10 f4 in 
                        k10 q9 max9 min9 f3 in 
             let k5 q4 max4 min4 res1 =
               k4 q4 max4 min4 res1 in 
             f1 k5 q3 max3 min3 f2 in 
  let rec compute k35 q34 max34 min34 vv =
    let f9 = fun k36 q35 max35 min35 bound1 ->
               let f10 = fun k37 q36 max36 min36 inc ->
                           let x22 = () in 
                           let k38 q37 max37 min37 x21 =
                             let x23 = vv = bound1 in 
                             let k39 q38 max38 min38 res23 =
                               let x20 = x22 ; res23 in  k37 q38 max38 min38 x20 in 
                             let k40 q39 max39 min39 res24 =
                               let x24 = 0 in 
                               k39 q39 max39 min39 x24 in 
                             let k41 q40 max40 min40 res25 =
                               let k45 q44 max44 min44 res29 =
                                 let k44 q43 max43 min43 res28 =
                                   let k43 q42 max42 min42 res27 =
                                     let k42 q41 max41 min41 res26 =
                                       k39 q41 max41 min41 res26 in 
                                     res27 k42 q42 max42 min42 inc in 
                                   res28 k43 q43 max43 min43 bound1 in 
                                 compute k44 q44 max44 min44 res29 in 
                               inc k45 q40 max40 min40 vv in 
                             if x23 then k40 q37 max37 min37 x23 else k41 q37 max37 min37 x23 in 
                           ev k38 q36 max36 min36 vv in 
               k36 q35 max35 min35 f10 in 
    k35 q34 max34 min34 f9 in 
  let k3 q2 max2 min2 res0 =
    let k46 q max min x25 =
      let x27 = max + min in  let x28 = 0 in 
                              let x26 = x27 = x28 in  assert(x26);x25 in 
    k46 q2 max2 min2 res0 in 
  f0 k3 q1 max1 min1 compute
