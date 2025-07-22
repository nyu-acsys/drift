(* CPS conversion. Source Program: 

let sell n1 = ev 2; ev n1; ()

let buy n2 = ev 1; ev n2; ()

let activity f n3 = f n3; ev 0; ()

let rec repeat n4 = 
  if (n4<=0) then 0 
  else 
    begin
      activity buy 1; 
      activity sell 1; 
      1 + repeat (n4-1)
    end

let main (budget:int(*-:{v:Int | v >= 0}*)) (stock:int(*-:{v:Int | v >= 0}*)) (n:int(*-:{v:Int | v > 0}*)) =
  if (stock >= budget && budget >= n) then
    begin
      activity sell (-stock);
      activity buy (-budget);
      repeat n
    end
  else
    0


Property: 

QSet   = [0;1;2;3]; 

(* [stable state, consumer state, producer state, error state] *)

delta  = fun evx (q, (budget, stock)) -> 
    if (q=0 && evx > 0 && evx < 3) then (evx, (budget, stock))
    else if (q=1 && evx<>0) then (q, ((budget-evx), stock))
    else if (q=2 && evx<>0) then (q, (budget, (stock-evx)))
    else if ((q=1||q=2) && evx=0) then (0, (budget, stock))
    else (3, (budget, stock));

IniCfg = (0, (0,0));

assert = fun (q, (budget, stock)) -> q < 3 && budget >=0 && stock >=0 && stock >= budget;

*)

let main prefstock prefn prefbudget = 
  let ev = fun k0 q budget stock evx ->
             if (((q = 0) && (evx > 0)) && (evx < 3)) then k0 evx budget stock () 
             else if ((q = 1) && (evx <> 0)) then k0 q (budget - evx) stock () 
                  else if ((q = 2) && (evx <> 0)) then k0 q budget (stock - evx) () 
                       else if (((q = 1) || (q = 2)) && (evx = 0)) then k0 0 budget stock () 
                            else k0 3 budget stock () in 
  let ev_assert = fun k1 q0 budget0 stock0 x0 ->
                    let k46 q budget stock x43 =
                      let x48 = 3 in 
                      let x47 = q < x48 in 
                      let x50 = 0 in 
                      let x49 = budget >= x50 in 
                      let x46 = x47 && x49 in 
                      let x52 = 0 in 
                      let x51 = stock >= x52 in  let x45 = x46 && x51 in  let x53 = stock >= budget in  let x44 = x45 && x53 in  let x42 = () in 
                                                                                                                                 assert(x44);k1 q budget stock x42 in 
                    ev k46 q0 budget0 stock0 x0 in 
  let q1 = 0 in 
  let budget1 = 0 in 
  let stock1 = 0 in 
  let f0 = fun k4 q3 budget3 stock3 sell ->
             let f1 = fun k6 q5 budget5 stock5 buy ->
                        let f2 = fun k8 q7 budget7 stock7 activity ->
                                   let f3 = fun k10 q9 budget9 stock9 repeat ->
                                              let f4 = fun k12 q11 budget11 stock11 _main ->
                                                         let k15 q14 budget14 stock14 res7 =
                                                           let k14 q13 budget13 stock13 res6 =
                                                             let k13 q12 budget12 stock12 res5 =
                                                               k12 q12 budget12 stock12 res5 in 
                                                             res6 k13 q13 budget13 stock13 prefn in 
                                                           res7 k14 q14 budget14 stock14 prefstock in 
                                                         _main k15 q11 budget11 stock11 prefbudget in 
                                              let f5 = fun k16 q15 budget15 stock15 budget ->
                                                         let f6 = fun k17 q16 budget16 stock16 stock ->
                                                                    let f7 = fun k18 q17 budget17 stock17 n ->
                                                                               let x2 = stock >= budget in 
                                                                               let x3 = budget >= n in 
                                                                               let x1 = x2 && x3 in 
                                                                               let k19 q18 budget18 stock18 res8 =
                                                                                 k18 q18 budget18 stock18 res8 in 
                                                                               let k20 q19 budget19 stock19 res9 =
                                                                                 let k23 q22 budget22 stock22 res12 =
                                                                                   let x6 = - stock in 
                                                                                   let k22 q21 budget21 stock21 res11 =
                                                                                     let k25 q24 budget24 stock24 res14 =
                                                                                       let x8 = - budget in 
                                                                                       let k24 q23 budget23 stock23 res13 =
                                                                                         let k26 q25 budget25 stock25 res15 =
                                                                                           let x7 = res13 ; res15 in  let x5 = res11 ; x7 in  k19 q25 budget25 stock25 x5 in 
                                                                                         repeat k26 q23 budget23 stock23 n in 
                                                                                       res14 k24 q24 budget24 stock24 x8 in 
                                                                                     activity k25 q21 budget21 stock21 buy in 
                                                                                   res12 k22 q22 budget22 stock22 x6 in 
                                                                                 activity k23 q19 budget19 stock19 sell in 
                                                                               let k21 q20 budget20 stock20 res10 =
                                                                                 let x4 = 0 in 
                                                                                 k19 q20 budget20 stock20 x4 in 
                                                                               if x1 then k20 q17 budget17 stock17 x1 else k21 q17 budget17 stock17 x1 in 
                                                                    k17 q16 budget16 stock16 f7 in 
                                                         k16 q15 budget15 stock15 f6 in 
                                              let k11 q10 budget10 stock10 res4 =
                                                k10 q10 budget10 stock10 res4 in 
                                              f4 k11 q9 budget9 stock9 f5 in 
                                   let rec repeat k27 q26 budget26 stock26 n4 =
                                     let x10 = 0 in 
                                     let x9 = n4 <= x10 in 
                                     let k28 q27 budget27 stock27 res16 =
                                       k27 q27 budget27 stock27 res16 in 
                                     let k29 q28 budget28 stock28 res17 =
                                       let x19 = 0 in 
                                       k28 q28 budget28 stock28 x19 in 
                                     let k30 q29 budget29 stock29 res18 =
                                       let k32 q31 budget31 stock31 res20 =
                                         let x12 = 1 in 
                                         let k31 q30 budget30 stock30 res19 =
                                           let k34 q33 budget33 stock33 res22 =
                                             let x14 = 1 in 
                                             let k33 q32 budget32 stock32 res21 =
                                               let x16 = 1 in 
                                               let x18 = 1 in 
                                               let x17 = n4 - x18 in 
                                               let k35 q34 budget34 stock34 res23 =
                                                 let x15 = x16 + res23 in  let x13 = res21 ; x15 in  let x11 = res19 ; x13 in  k28 q34 budget34 stock34 x11 in 
                                               repeat k35 q32 budget32 stock32 x17 in 
                                             res22 k33 q33 budget33 stock33 x14 in 
                                           activity k34 q30 budget30 stock30 sell in 
                                         res20 k31 q31 budget31 stock31 x12 in 
                                       activity k32 q29 budget29 stock29 buy in 
                                     if x9 then k29 q26 budget26 stock26 x9 else k30 q26 budget26 stock26 x9 in 
                                   let k9 q8 budget8 stock8 res3 =
                                     k8 q8 budget8 stock8 res3 in 
                                   f3 k9 q7 budget7 stock7 repeat in 
                        let f8 = fun k36 q35 budget35 stock35 f ->
                                   let f9 = fun k37 q36 budget36 stock36 n3 ->
                                              let k38 q37 budget37 stock37 res24 =
                                                let x24 = 0 in 
                                                let x23 = () in 
                                                let k39 q38 budget38 stock38 x22 =
                                                  let x25 = () in 
                                                  let x21 = x23 ; x25 in  let x20 = res24 ; x21 in  k37 q38 budget38 stock38 x20 in 
                                                ev_assert k39 q37 budget37 stock37 x24 in 
                                              f k38 q36 budget36 stock36 n3 in 
                                   k36 q35 budget35 stock35 f9 in 
                        let k7 q6 budget6 stock6 res2 =
                          k6 q6 budget6 stock6 res2 in 
                        f2 k7 q5 budget5 stock5 f8 in 
             let f10 = fun k40 q39 budget39 stock39 n2 ->
                         let x29 = 1 in 
                         let x28 = () in 
                         let k41 q40 budget40 stock40 x27 =
                           let x32 = () in 
                           let k42 q41 budget41 stock41 x31 =
                             let x33 = () in 
                             let x30 = x32 ; x33 in  let x26 = x28 ; x30 in  k40 q41 budget41 stock41 x26 in 
                           ev_assert k42 q40 budget40 stock40 n2 in 
                         ev_assert k41 q39 budget39 stock39 x29 in 
             let k5 q4 budget4 stock4 res1 =
               k4 q4 budget4 stock4 res1 in 
             f1 k5 q3 budget3 stock3 f10 in 
  let f11 = fun k43 q42 budget42 stock42 n1 ->
              let x37 = 2 in 
              let x36 = () in 
              let k44 q43 budget43 stock43 x35 =
                let x40 = () in 
                let k45 q44 budget44 stock44 x39 =
                  let x41 = () in 
                  let x38 = x40 ; x41 in  let x34 = x36 ; x38 in  k43 q44 budget44 stock44 x34 in 
                ev_assert k45 q43 budget43 stock43 n1 in 
              ev_assert k44 q42 budget42 stock42 x37 in 
  let k3 q2 budget2 stock2 res0 =
    res0 in 
  f0 k3 q1 budget1 stock1 f11