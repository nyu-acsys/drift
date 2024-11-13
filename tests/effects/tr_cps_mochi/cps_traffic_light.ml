(* CPS conversion. Source Program: 

let next x = 
  ev x;
  x - 1 

let rec loop s = 
  if (s > 0) then loop (next s)
  else ()

let main (u: unit(*-:{v:Unit | unit}*)) = 
  ev 0;
  loop 4



Property: 

(* ============================================================================================ *)
(* ===                         III. 3-State AUTOMATON & Initial CFG                         === *)
(* ============================================================================================ *)

QSet   = [0; 1; 2; 3]; 

delta  = fun evx (q, (max, min)) -> 
       if evx < min then (q, (max,evx)) 
	   else if evx > max then (q, (evx,min)) 
	   else (q, (max,min)); 

IniCfg = (0, (0, 0));

assertFinal = fun (q, (max, min)) -> max + min = 0;



*)

let main prefu = 
  let ev = fun k0 q max min evx ->
             if (evx < min) then k0 q max evx () 
             else if (evx > max) then k0 q evx min () 
                  else k0 q max min () in 
  let q1 = 0 in 
  let max1 = 0 in 
  let min1 = 0 in 
  let f0 = fun k4 q3 max3 min3 next ->
             let f1 = fun k6 q5 max5 min5 loop ->
                        let f2 = fun k8 q7 max7 min7 _main ->
                                   let k9 q8 max8 min8 res3 =
                                     k8 q8 max8 min8 res3 in 
                                   _main k9 q7 max7 min7 prefu in 
                        let f3 = fun k10 q9 max9 min9 u ->
                                   let x4 = 0 in 
                                   let x3 = () in 
                                   let k11 q10 max10 min10 x2 =
                                     let x5 = 4 in 
                                     let k12 q11 max11 min11 res4 =
                                       let x1 = x3 ; res4 in  k10 q11 max11 min11 x1 in 
                                     loop k12 q10 max10 min10 x5 in 
                                   ev k11 q9 max9 min9 x4 in 
                        let k7 q6 max6 min6 res2 =
                          k6 q6 max6 min6 res2 in 
                        f2 k7 q5 max5 min5 f3 in 
             let rec loop k13 q12 max12 min12 s =
               let x7 = 0 in 
               let x6 = s > x7 in 
               let k14 q13 max13 min13 res5 =
                 k13 q13 max13 min13 res5 in 
               let k15 q14 max14 min14 res6 =
                 let k18 q17 max17 min17 res9 =
                   let k17 q16 max16 min16 res8 =
                     k14 q16 max16 min16 res8 in 
                   loop k17 q17 max17 min17 res9 in 
                 next k18 q14 max14 min14 s in 
               let k16 q15 max15 min15 res7 =
                 let x8 = () in 
                 k14 q15 max15 min15 x8 in 
               if x6 then k15 q12 max12 min12 x6 else k16 q12 max12 min12 x6 in 
             let k5 q4 max4 min4 res1 =
               k4 q4 max4 min4 res1 in 
             f1 k5 q3 max3 min3 loop in 
  let f4 = fun k19 q18 max18 min18 x ->
             let x11 = () in 
             let k20 q19 max19 min19 x10 =
               let x13 = 1 in 
               let x12 = x - x13 in  let x9 = x11 ; x12 in  k19 q19 max19 min19 x9 in 
             ev k20 q18 max18 min18 x in 
  let k3 q2 max2 min2 res0 =
    let k21 q max min x14 =
      let x16 = max + min in  let x17 = 0 in 
                              let x15 = x16 = x17 in  assert(x15);x14 in 
    k21 q2 max2 min2 res0 in 
  f0 k3 q1 max1 min1 f4