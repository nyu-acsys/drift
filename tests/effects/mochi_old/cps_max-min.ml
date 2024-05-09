(* CPS conversion. Source Program: 


let rec compute vv bound inc = 
    if vv = bound then 0 else begin
      ev vv; 
      compute (inc vv) bound inc
    end

let main (v:int(*-:{v:Int | true}*)) (w:int(*-:{v:Int | true}*)) (m:int(*-:{v:Int | true}*)) =
  if (v>0) then
    let bound = -1 * v in
    compute v bound (fun t -> t-1)
  else 
    let bound = -1 * v in
    compute v bound (fun t -> t+1)


Property: 

(* Max == -1 * Min *)
 
QSet   = [0;1]; 

delta  = fun evx (q, (max,min)) -> 
   if evx < min then (1, (max,evx)) 
   else if evx > max then (1, (evx,min)) 
   else (1, (max,min));

IniCfg = (0, (0,0));

assertFinal = fun (q, (max,min)) -> max + min = 0;


*)

let main prefw prefv prefm = 
  let ev = fun k0 q max min evx ->
             if (evx < min) then k0 1 max evx () 
             else if (evx > max) then k0 1 evx min () 
                  else k0 1 max min () in 
  let q1 = 0 in 
  let max1 = 0 in 
  let min1 = 0 in 
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
                                              let x2 = 0 in 
                                              let x1 = v > x2 in 
                                              let k13 q12 max12 min12 res5 =
                                                k12 q12 max12 min12 res5 in 
                                              let k14 q13 max13 min13 res6 =
                                                let f7 = fun k23 q22 max22 min22 bound ->
                                                           let k26 q25 max25 min25 res15 =
                                                             let k25 q24 max24 min24 res14 =
                                                               let f8 = fun k27 q26 max26 min26 t ->
                                                                          let x8 = 1 in 
                                                                          let x7 = t - x8 in  k27 q26 max26 min26 x7 in 
                                                               let k24 q23 max23 min23 res13 =
                                                                 k23 q23 max23 min23 res13 in 
                                                               res14 k24 q24 max24 min24 f8 in 
                                                             res15 k25 q25 max25 min25 bound in 
                                                           compute k26 q22 max22 min22 v in 
                                                let x10 = -1 in 
                                                let x9 = x10 * v in  let k22 q21 max21 min21 res12 =
                                                                       k13 q21 max21 min21 res12 in 
                                                                     f7 k22 q13 max13 min13 x9 in 
                                              let k15 q14 max14 min14 res7 =
                                                let f5 = fun k17 q16 max16 min16 bound ->
                                                           let k20 q19 max19 min19 res11 =
                                                             let k19 q18 max18 min18 res10 =
                                                               let f6 = fun k21 q20 max20 min20 t ->
                                                                          let x4 = 1 in 
                                                                          let x3 = t + x4 in  k21 q20 max20 min20 x3 in 
                                                               let k18 q17 max17 min17 res9 =
                                                                 k17 q17 max17 min17 res9 in 
                                                               res10 k18 q18 max18 min18 f6 in 
                                                             res11 k19 q19 max19 min19 bound in 
                                                           compute k20 q16 max16 min16 v in 
                                                let x6 = -1 in 
                                                let x5 = x6 * v in  let k16 q15 max15 min15 res8 =
                                                                      k13 q15 max15 min15 res8 in 
                                                                    f5 k16 q14 max14 min14 x5 in 
                                              if x1 then k14 q11 max11 min11 x1 else k15 q11 max11 min11 x1 in 
                                   k11 q10 max10 min10 f4 in 
                        k10 q9 max9 min9 f3 in 
             let k5 q4 max4 min4 res1 =
               k4 q4 max4 min4 res1 in 
             f1 k5 q3 max3 min3 f2 in 
  let rec compute k28 q27 max27 min27 vv =
    let f9 = fun k29 q28 max28 min28 bound ->
               let f10 = fun k30 q29 max29 min29 inc ->
                           let x11 = vv = bound in 
                           let k31 q30 max30 min30 res16 =
                             k30 q30 max30 min30 res16 in 
                           let k32 q31 max31 min31 res17 =
                             let x15 = 0 in 
                             k31 q31 max31 min31 x15 in 
                           let k33 q32 max32 min32 res18 =
                             let x14 = () in 
                             let k34 q33 max33 min33 x13 =
                               let k38 q37 max37 min37 res22 =
                                 let k37 q36 max36 min36 res21 =
                                   let k36 q35 max35 min35 res20 =
                                     let k35 q34 max34 min34 res19 =
                                       let x12 = x14 ; res19 in  k31 q34 max34 min34 x12 in 
                                     res20 k35 q35 max35 min35 inc in 
                                   res21 k36 q36 max36 min36 bound in 
                                 compute k37 q37 max37 min37 res22 in 
                               inc k38 q33 max33 min33 vv in 
                             ev k34 q32 max32 min32 vv in 
                           if x11 then k32 q29 max29 min29 x11 else k33 q29 max29 min29 x11 in 
               k29 q28 max28 min28 f10 in 
    k28 q27 max27 min27 f9 in 
  let k3 q2 max2 min2 res0 =
    let k39 q max min x16 =
      let x18 = max + min in  let x19 = 0 in 
                              let x17 = x18 = x19 in  assert(x17);x16 in 
    k39 q2 max2 min2 res0 in 
  f0 k3 q1 max1 min1 compute