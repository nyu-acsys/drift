(* CPS conversion. Source Program: 

let rec reent d = 
  ev 1; (* acquire *) 
  if (d > 0) then 
    begin
      if nondet then begin reent (d-1); ev (-1) (* release *) end
      else ()
    end
  else 
    ()

let main (n: int(*-:{v:Int | v > 0}*)) = 
  reent n; ev (-1) (* release *)



Property: 

(* Never release more than acquire *)

QSet = [0;1];

delta = fun evx (q, acc) -> 
      if q = 0 && (acc + evx) >= 0 then (q, acc + evx)
      else if q = 0 && (acc + evx) < 0 then (1, acc + evx)
      else (q, acc);

IniCfg = (0, 0);

assert = fun (q, acc) -> q = 0;
   


*)

let main prefn = 
  let ev = fun k0 q acc evx ->
             if ((q = 0) && ((acc + evx) >= 0)) then k0 q (acc + evx) () 
             else if ((q = 0) && ((acc + evx) < 0)) then k0 1 (acc + evx) () 
                  else k0 q acc () in 
  let ev_assert = fun k1 q0 acc0 x0 ->
                    let k22 q acc x22 =
                      let x24 = 0 in 
                      let x23 = q = x24 in  let x21 = () in 
                                            assert(x23);k1 q acc x21 in 
                    ev k22 q0 acc0 x0 in 
  let q1 = 0 in 
  let acc1 = 0 in 
  let f0 = fun k4 q3 acc3 reent ->
             let f1 = fun k6 q5 acc5 _main ->
                        let k7 q6 acc6 res2 =
                          k6 q6 acc6 res2 in 
                        _main k7 q5 acc5 prefn in 
             let f2 = fun k8 q7 acc7 n ->
                        let k9 q8 acc8 res3 =
                          let x4 = -1 in 
                          let x3 = () in 
                          let k10 q9 acc9 x2 =
                            let x1 = res3 ; x3 in  k8 q9 acc9 x1 in 
                          ev_assert k10 q8 acc8 x4 in 
                        reent k9 q7 acc7 n in 
             let k5 q4 acc4 res1 =
               k4 q4 acc4 res1 in 
             f1 k5 q3 acc3 f2 in 
  let rec reent k11 q10 acc10 d =
    let x8 = 1 in 
    let x7 = () in 
    let k12 q11 acc11 x6 =
      let x10 = 0 in 
      let x9 = d > x10 in 
      let k13 q12 acc12 res4 =
        let x5 = x7 ; res4 in  k11 q12 acc12 x5 in 
      let k14 q13 acc13 res5 =
        let x12 = Random.int(0) in 
        let x13 = 0 in 
        let bool_random0 = x12 >= x13 in 
        let k16 q15 acc15 res7 =
          k13 q15 acc15 res7 in 
        let k17 q16 acc16 res8 =
          let x17 = 1 in 
          let x16 = d - x17 in 
          let k20 q19 acc19 res11 =
            let x20 = -1 in 
            let x19 = () in 
            let k21 q20 acc20 x18 =
              let x15 = res11 ; x19 in  k16 q20 acc20 x15 in 
            ev_assert k21 q19 acc19 x20 in 
          reent k20 q16 acc16 x16 in 
        let k18 q17 acc17 res9 =
          let x14 = () in 
          k16 q17 acc17 x14 in 
        if bool_random0 then k17 q13 acc13 bool_random0 else k18 q13 acc13 bool_random0 in 
      let k15 q14 acc14 res6 =
        let x11 = () in 
        k13 q14 acc14 x11 in 
      if x9 then k14 q11 acc11 x9 else k15 q11 acc11 x9 in 
    ev_assert k12 q10 acc10 x8 in 
  let k3 q2 acc2 res0 =
    res0 in 
  f0 k3 q1 acc1 reent