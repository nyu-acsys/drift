(* CPS conversion. Source Program: 

let add_stock n6 = ev 2; ev (-n6); ()

let add_money n5 = ev 1; ev (-n5); ()

let sell n4 = ev 2; ev n4; ()

let buy n3 = ev 1; ev n3; ()

let activity f n2 = f n2; ev 0; ()

let rec repeat n1 = 
  if (n1<=0) then 0 
  else 
    begin
      activity buy 1; 
      activity sell 1; 
      1 + repeat (n1-1)
    end

let main (budget:int(*-:{v:Int | true}*)) (stock:int(*-:{v:Int | true}*)) (n:int(*-:{v:Int | true}*)) =
  if (stock >= budget && budget >= n && n>0) then
    begin
      activity add_stock stock;
      activity add_money budget;
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
             else if ((q = 1) && (evx != 0)) then k0 q (budget - evx) stock () 
                  else if ((q = 2) && (evx != 0)) then k0 q budget (stock - evx) () 
                       else if (((q = 1) || (q = 2)) && (evx = 0)) then k0 0 budget stock () 
                            else k0 3 budget stock () in 
  let ev_assert = fun k1 q0 budget0 stock0 x0 ->
                    let k56 q budget stock x62 =
                      let x67 = 3 in 
                      let x66 = q < x67 in 
                      let x69 = 0 in 
                      let x68 = budget >= x69 in 
                      let x65 = x66 && x68 in 
                      let x71 = 0 in 
                      let x70 = stock >= x71 in  let x64 = x65 && x70 in  let x72 = stock >= budget in  let x63 = x64 && x72 in  let x61 = () in 
                                                                                                                                 assert(x63);k1 q budget stock x61 in 
                    ev k56 q0 budget0 stock0 x0 in 
  let q1 = 0 in 
  let budget1 = 0 in 
  let stock1 = 0 in 
  let f0 = fun k4 q3 budget3 stock3 add_stock ->
             let f1 = fun k6 q5 budget5 stock5 add_money ->
                        let f2 = fun k8 q7 budget7 stock7 sell ->
                                   let f3 = fun k10 q9 budget9 stock9 buy ->
                                              let f4 = fun k12 q11 budget11 stock11 activity ->
                                                         let f5 = fun k14 q13 budget13 stock13 repeat ->
                                                                    let f6 = fun k16 q15 budget15 stock15 _main ->
                                                                               let k19 q18 budget18 stock18 res9 =
                                                                                 let k18 q17 budget17 stock17 res8 =
                                                                                   let k17 q16 budget16 stock16 res7 =
                                                                                     k16 q16 budget16 stock16 res7 in 
                                                                                   res8 k17 q17 budget17 stock17 prefn in 
                                                                                 res9 k18 q18 budget18 stock18 prefstock in 
                                                                               _main k19 q15 budget15 stock15 prefbudget in 
                                                                    let f7 = fun k20 q19 budget19 stock19 budget ->
                                                                               let f8 = fun k21 q20 budget20 stock20 stock ->
                                                                                          let f9 = fun k22 q21 budget21 stock21 n ->
                                                                                                     let x2 = stock >= budget in 
                                                                                                     let x4 = budget >= n in 
                                                                                                     let x6 = 0 in 
                                                                                                     let x5 = n > x6 in 
                                                                                                     let x3 = x4 && x5 in 
                                                                                                     let x1 = x2 && x3 in 
                                                                                                     let k23 q22 budget22 stock22 res10 =
                                                                                                       k22 q22 budget22 stock22 res10 in 
                                                                                                     let k24 q23 budget23 stock23 res11 =
                                                                                                       let k27 q26 budget26 stock26 res14 =
                                                                                                         let k26 q25 budget25 stock25 res13 =
                                                                                                           let k29 q28 budget28 stock28 res16 =
                                                                                                             let k28 q27 budget27 stock27 res15 =
                                                                                                               let k30 q29 budget29 stock29 res17 =
                                                                                                                 let x9 = res15 ; res17 in  let x8 = res13 ; x9 in  k23 q29 budget29 stock29 x8 in 
                                                                                                               repeat k30 q27 budget27 stock27 n in 
                                                                                                             res16 k28 q28 budget28 stock28 budget in 
                                                                                                           activity k29 q25 budget25 stock25 add_money in 
                                                                                                         res14 k26 q26 budget26 stock26 stock in 
                                                                                                       activity k27 q23 budget23 stock23 add_stock in 
                                                                                                     let k25 q24 budget24 stock24 res12 =
                                                                                                       let x7 = 0 in 
                                                                                                       k23 q24 budget24 stock24 x7 in 
                                                                                                     if x1 then k24 q21 budget21 stock21 x1 else k25 q21 budget21 stock21 x1 in 
                                                                                          k21 q20 budget20 stock20 f9 in 
                                                                               k20 q19 budget19 stock19 f8 in 
                                                                    let k15 q14 budget14 stock14 res6 =
                                                                      k14 q14 budget14 stock14 res6 in 
                                                                    f6 k15 q13 budget13 stock13 f7 in 
                                                         let rec repeat k31 q30 budget30 stock30 n1 =
                                                           let x11 = 0 in 
                                                           let x10 = n1 <= x11 in 
                                                           let k32 q31 budget31 stock31 res18 =
                                                             k31 q31 budget31 stock31 res18 in 
                                                           let k33 q32 budget32 stock32 res19 =
                                                             let x20 = 0 in 
                                                             k32 q32 budget32 stock32 x20 in 
                                                           let k34 q33 budget33 stock33 res20 =
                                                             let k36 q35 budget35 stock35 res22 =
                                                               let x13 = 1 in 
                                                               let k35 q34 budget34 stock34 res21 =
                                                                 let k38 q37 budget37 stock37 res24 =
                                                                   let x15 = 1 in 
                                                                   let k37 q36 budget36 stock36 res23 =
                                                                     let x17 = 1 in 
                                                                     let x19 = 1 in 
                                                                     let x18 = n1 - x19 in 
                                                                     let k39 q38 budget38 stock38 res25 =
                                                                       let x16 = x17 + res25 in  let x14 = res23 ; x16 in  let x12 = res21 ; x14 in  k32 q38 budget38 stock38 x12 in 
                                                                     repeat k39 q36 budget36 stock36 x18 in 
                                                                   res24 k37 q37 budget37 stock37 x15 in 
                                                                 activity k38 q34 budget34 stock34 sell in 
                                                               res22 k35 q35 budget35 stock35 x13 in 
                                                             activity k36 q33 budget33 stock33 buy in 
                                                           if x10 then k33 q30 budget30 stock30 x10 else k34 q30 budget30 stock30 x10 in 
                                                         let k13 q12 budget12 stock12 res5 =
                                                           k12 q12 budget12 stock12 res5 in 
                                                         f5 k13 q11 budget11 stock11 repeat in 
                                              let f10 = fun k40 q39 budget39 stock39 f ->
                                                          let f11 = fun k41 q40 budget40 stock40 n2 ->
                                                                      let k42 q41 budget41 stock41 res26 =
                                                                        let x25 = 0 in 
                                                                        let x24 = () in 
                                                                        let k43 q42 budget42 stock42 x23 =
                                                                          let x26 = () in 
                                                                          let x22 = x24 ; x26 in  let x21 = res26 ; x22 in  k41 q42 budget42 stock42 x21 in 
                                                                        ev_assert k43 q41 budget41 stock41 x25 in 
                                                                      f k42 q40 budget40 stock40 n2 in 
                                                          k40 q39 budget39 stock39 f11 in 
                                              let k11 q10 budget10 stock10 res4 =
                                                k10 q10 budget10 stock10 res4 in 
                                              f4 k11 q9 budget9 stock9 f10 in 
                                   let f12 = fun k44 q43 budget43 stock43 n3 ->
                                               let x30 = 1 in 
                                               let x29 = () in 
                                               let k45 q44 budget44 stock44 x28 =
                                                 let x33 = () in 
                                                 let k46 q45 budget45 stock45 x32 =
                                                   let x34 = () in 
                                                   let x31 = x33 ; x34 in  let x27 = x29 ; x31 in  k44 q45 budget45 stock45 x27 in 
                                                 ev_assert k46 q44 budget44 stock44 n3 in 
                                               ev_assert k45 q43 budget43 stock43 x30 in 
                                   let k9 q8 budget8 stock8 res3 =
                                     k8 q8 budget8 stock8 res3 in 
                                   f3 k9 q7 budget7 stock7 f12 in 
                        let f13 = fun k47 q46 budget46 stock46 n4 ->
                                    let x38 = 2 in 
                                    let x37 = () in 
                                    let k48 q47 budget47 stock47 x36 =
                                      let x41 = () in 
                                      let k49 q48 budget48 stock48 x40 =
                                        let x42 = () in 
                                        let x39 = x41 ; x42 in  let x35 = x37 ; x39 in  k47 q48 budget48 stock48 x35 in 
                                      ev_assert k49 q47 budget47 stock47 n4 in 
                                    ev_assert k48 q46 budget46 stock46 x38 in 
                        let k7 q6 budget6 stock6 res2 =
                          k6 q6 budget6 stock6 res2 in 
                        f2 k7 q5 budget5 stock5 f13 in 
             let f14 = fun k50 q49 budget49 stock49 n5 ->
                         let x46 = 1 in 
                         let x45 = () in 
                         let k51 q50 budget50 stock50 x44 =
                           let x50 = - n5 in 
                           let x49 = () in 
                           let k52 q51 budget51 stock51 x48 =
                             let x51 = () in 
                             let x47 = x49 ; x51 in  let x43 = x45 ; x47 in  k50 q51 budget51 stock51 x43 in 
                           ev_assert k52 q50 budget50 stock50 x50 in 
                         ev_assert k51 q49 budget49 stock49 x46 in 
             let k5 q4 budget4 stock4 res1 =
               k4 q4 budget4 stock4 res1 in 
             f1 k5 q3 budget3 stock3 f14 in 
  let f15 = fun k53 q52 budget52 stock52 n6 ->
              let x55 = 2 in 
              let x54 = () in 
              let k54 q53 budget53 stock53 x53 =
                let x59 = - n6 in 
                let x58 = () in 
                let k55 q54 budget54 stock54 x57 =
                  let x60 = () in 
                  let x56 = x58 ; x60 in  let x52 = x54 ; x56 in  k53 q54 budget54 stock54 x52 in 
                ev_assert k55 q53 budget53 stock53 x59 in 
              ev_assert k54 q52 budget52 stock52 x55 in 
  let k3 q2 budget2 stock2 res0 =
    res0 in 
  f0 k3 q1 budget1 stock1 f15