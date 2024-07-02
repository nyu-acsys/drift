(* CPS conversion. Source Program: 

(* Adapted from:
   A Fixpoint Logic and Dependent Effects for
   Temporal Property Verification
   Nanjo et al, LICS 2018 *)

let rec listener i npool pend =
  if nondet && pend < npool then begin
    ev 1; (* Accept *)
    listener i npool (pend + 1)
  end else if pend > 0 then begin
    ev 2; (* Handle *)
    listener i npool (pend - 1)
  end else begin
    ev 3; (* Wait *)
    listener i npool pend
  end

let main (npool:int(*-:{v:Int | true}*)) (i0:int(*-:{v:Int | true}*)) =
  listener i0 npool 0


Property: 

(* From LICS18:
"One critical property is that every accepted connection is eventually handled, i.e., that the pool of pending clients eventually becomes empty. This is, however, not true in general since infinitely many new clients may preempt handling pending clients. The property must be instead weakened to include a fairness constraint"

  accept(1), handle(2), wait(3)

Finite version of the property:
  keep track of number pending.
  After 42 events,
     "fair" if you only see handle() events, until 0 pending.
     "unfair" if you see other things, loop at unfair state q666

       (q0)--[accept; pending++; tick++]-->(q0)
       (q0)--[handle; pending--; tick++]-->(q0)
       (q0)--[_; tick==42]-->(q42)

     From 42, non-handle events bring you to unfair state q666:
              handle events loop but reduce pending
	      if pending is now 0, go to q0
*)

QSet = [0;42;666];

delta = fun evx (q, (pending,tick)) ->
  (* ways to loop at q0 *)
  if      (q = 0 && tick < 42 && evx=1) then (0, (pending+1,tick+1))
  else if (q = 0 && tick < 42 && evx=2) then (0, (pending-1,tick+1))
  else if (q = 0 && tick < 42 && evx=3) then (0, (pending,  tick+1))
  (* ways to move to q42 *)
  else if (q = 0 && tick = 42 && evx=1) then (42, (pending+1,43))
  else if (q = 0 && tick = 42 && evx=2) then (42, (pending-1,43))
  else if (q = 0 && tick = 42 && evx=3) then (42, (pending,  43))
  (* in q42 but get Handle events *)
  else if (q = 42 && evx=2 && pending = 1) then (0, (0,0))
  else if (q = 42 && evx=2 )               then (42, (pending-1,tick-1))
  (* in q42 but get unfair events *)
  else if (q = 42)                        then (666, (0,0))
  (* self-loop at 666 *)
  else if (q = 666)                       then (666, (0,0))
  (* Not possible: *)
  else (q, (pending, tick));

IniCfg = (0, (0,0));

assert = fun (q, (pending, tick)) -> q = 666 || pending <= tick;


*)

let main prefnpool prefi0 = 
  let ev = fun k0 q pending tick evx ->
             if (((q = 0) && (tick < 42)) && (evx = 1)) then k0 0 (pending + 1) (tick + 1) () 
             else if (((q = 0) && (tick < 42)) && (evx = 2)) then k0 0 (pending - 1) (tick + 1) () 
                  else if (((q = 0) && (tick < 42)) && (evx = 3)) then k0 0 pending (tick + 1) () 
                       else if (((q = 0) && (tick = 42)) && (evx = 1)) then k0 42 (pending + 1) 43 () 
                            else if (((q = 0) && (tick = 42)) && (evx = 2)) then k0 42 (pending - 1) 43 () 
                                 else if (((q = 0) && (tick = 42)) && (evx = 3)) then k0 42 pending 43 () 
                                      else if (((q = 42) && (evx = 2)) && (pending = 1)) then k0 0 0 0 () 
                                           else if ((q = 42) && (evx = 2)) then k0 42 (pending - 1) (tick - 1) () 
                                                else if (q = 42) then k0 666 0 0 () 
                                                     else if (q = 666) then k0 666 0 0 () 
                                                          else k0 q pending tick () in 
  let ev_assert = fun k1 q0 pending0 tick0 x0 ->
                    let k36 q pending tick x25 =
                      let x28 = 666 in 
                      let x27 = q = x28 in  let x29 = pending <= tick in  let x26 = x27 || x29 in  let x24 = () in 
                                                                                                   assert(x26);k1 q pending tick x24 in 
                    ev k36 q0 pending0 tick0 x0 in 
  let q1 = 0 in 
  let pending1 = 0 in 
  let tick1 = 0 in 
  let f0 = fun k4 q3 pending3 tick3 listener ->
             let f1 = fun k6 q5 pending5 tick5 _main ->
                        let k8 q7 pending7 tick7 res3 =
                          let k7 q6 pending6 tick6 res2 =
                            k6 q6 pending6 tick6 res2 in 
                          res3 k7 q7 pending7 tick7 prefi0 in 
                        _main k8 q5 pending5 tick5 prefnpool in 
             let f2 = fun k9 q8 pending8 tick8 npool ->
                        let f3 = fun k10 q9 pending9 tick9 i0 ->
                                   let k13 q12 pending12 tick12 res6 =
                                     let k12 q11 pending11 tick11 res5 =
                                       let x1 = 0 in 
                                       let k11 q10 pending10 tick10 res4 =
                                         k10 q10 pending10 tick10 res4 in 
                                       res5 k11 q11 pending11 tick11 x1 in 
                                     res6 k12 q12 pending12 tick12 npool in 
                                   listener k13 q9 pending9 tick9 i0 in 
                        k9 q8 pending8 tick8 f3 in 
             let k5 q4 pending4 tick4 res1 =
               k4 q4 pending4 tick4 res1 in 
             f1 k5 q3 pending3 tick3 f2 in 
  let rec listener k14 q13 pending13 tick13 i =
    let f4 = fun k15 q14 pending14 tick14 npool ->
               let f5 = fun k16 q15 pending15 tick15 pend ->
                          let x3 = Random.int(0) in 
                          let x4 = 0 in 
                          let bool_random0 = x3 >= x4 in 
                          let x5 = pend < npool in 
                          let x2 = bool_random0 && x5 in 
                          let k17 q16 pending16 tick16 res7 =
                            k16 q16 pending16 tick16 res7 in 
                          let k18 q17 pending17 tick17 res8 =
                            let x21 = 1 in 
                            let x20 = () in 
                            let k32 q31 pending31 tick31 x19 =
                              let k35 q34 pending34 tick34 res22 =
                                let k34 q33 pending33 tick33 res21 =
                                  let x23 = 1 in 
                                  let x22 = pend + x23 in  let k33 q32 pending32 tick32 res20 =
                                                             let x18 = x20 ; res20 in  k17 q32 pending32 tick32 x18 in 
                                                           res21 k33 q33 pending33 tick33 x22 in 
                                res22 k34 q34 pending34 tick34 npool in 
                              listener k35 q31 pending31 tick31 i in 
                            ev_assert k32 q17 pending17 tick17 x21 in 
                          let k19 q18 pending18 tick18 res9 =
                            let x7 = 0 in 
                            let x6 = pend > x7 in 
                            let k21 q20 pending20 tick20 res11 =
                              k17 q20 pending20 tick20 res11 in 
                            let k22 q21 pending21 tick21 res12 =
                              let x15 = 2 in 
                              let x14 = () in 
                              let k28 q27 pending27 tick27 x13 =
                                let k31 q30 pending30 tick30 res19 =
                                  let k30 q29 pending29 tick29 res18 =
                                    let x17 = 1 in 
                                    let x16 = pend - x17 in  let k29 q28 pending28 tick28 res17 =
                                                               let x12 = x14 ; res17 in  k21 q28 pending28 tick28 x12 in 
                                                             res18 k29 q29 pending29 tick29 x16 in 
                                  res19 k30 q30 pending30 tick30 npool in 
                                listener k31 q27 pending27 tick27 i in 
                              ev_assert k28 q21 pending21 tick21 x15 in 
                            let k23 q22 pending22 tick22 res13 =
                              let x11 = 3 in 
                              let x10 = () in 
                              let k24 q23 pending23 tick23 x9 =
                                let k27 q26 pending26 tick26 res16 =
                                  let k26 q25 pending25 tick25 res15 =
                                    let k25 q24 pending24 tick24 res14 =
                                      let x8 = x10 ; res14 in  k21 q24 pending24 tick24 x8 in 
                                    res15 k25 q25 pending25 tick25 pend in 
                                  res16 k26 q26 pending26 tick26 npool in 
                                listener k27 q23 pending23 tick23 i in 
                              ev_assert k24 q22 pending22 tick22 x11 in 
                            if x6 then k22 q18 pending18 tick18 x6 else k23 q18 pending18 tick18 x6 in 
                          if x2 then k18 q15 pending15 tick15 x2 else k19 q15 pending15 tick15 x2 in 
               k15 q14 pending14 tick14 f5 in 
    k14 q13 pending13 tick13 f4 in 
  let k3 q2 pending2 tick2 res0 =
    res0 in 
  f0 k3 q1 pending1 tick1 listener