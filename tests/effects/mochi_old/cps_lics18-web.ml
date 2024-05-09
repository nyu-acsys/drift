(* CPS conversion. Source Program: 

(* Adapted from:
   A Fixpoint Logic and Dependent Effects for
   Temporal Property Verification
   Nanjo et al, LICS 2018 *)

let nondet i =
  (i * i * i * 199) mod 2

let rec listener i npool pend =
  if nondet i = 0 && pend < npool then begin
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
  else if (q = 0 && tick = 42 && evx=1) then (42, (pending+1,0))
  else if (q = 0 && tick = 42 && evx=2) then (42, (pending-1,0))
  else if (q = 0 && tick = 42 && evx=3) then (42, (pending,  0))
  (* in q42 but get Handle events *)
  else if (q = 42 && evx=2 && pending = 1) then (1, (0,0))
  else if (q = 42 && evx=2 )               then (42, (pending-1,0))
  (* in q42 but get unfair events *)
  else if (q = 42)                        then (666, (0,0))
  (* self-loop at 666 *)
  else if (q = 666)                       then (666, (0,0))
  (* Not possible: *)
  else (q, (pending, tick));

IniCfg = (0, (0,0));

assert = fun (q, (pending, tick)) -> q = 666 || pending <= 42;

*)

let main prefnpool prefi0 = 
  let ev = fun k0 q pending tick evx ->
             if (((q = 0) && (tick < 42)) && (evx = 1)) then k0 0 (pending + 1) (tick + 1) () 
             else if (((q = 0) && (tick < 42)) && (evx = 2)) then k0 0 (pending - 1) (tick + 1) () 
                  else if (((q = 0) && (tick < 42)) && (evx = 3)) then k0 0 pending (tick + 1) () 
                       else if (((q = 0) && (tick = 42)) && (evx = 1)) then k0 42 (pending + 1) 0 () 
                            else if (((q = 0) && (tick = 42)) && (evx = 2)) then k0 42 (pending - 1) 0 () 
                                 else if (((q = 0) && (tick = 42)) && (evx = 3)) then k0 42 pending 0 () 
                                      else if (((q = 42) && (evx = 2)) && (pending = 1)) then k0 1 0 0 () 
                                           else if ((q = 42) && (evx = 2)) then k0 42 (pending - 1) 0 () 
                                                else if (q = 42) then k0 666 0 0 () 
                                                     else if (q = 666) then k0 666 0 0 () 
                                                          else k0 q pending tick () in 
  let ev_assert = fun k1 q0 pending0 tick0 x0 ->
                    let k39 q pending tick x31 =
                      let x34 = 666 in 
                      let x33 = q = x34 in  let x36 = 42 in 
                                            let x35 = pending <= x36 in  let x32 = x33 || x35 in  let x30 = () in 
                                                                                                  assert(x32);k1 q pending tick x30 in 
                    ev k39 q0 pending0 tick0 x0 in 
  let q1 = 0 in 
  let pending1 = 0 in 
  let tick1 = 0 in 
  let f0 = fun k4 q3 pending3 tick3 nondet ->
             let f1 = fun k6 q5 pending5 tick5 listener ->
                        let f2 = fun k8 q7 pending7 tick7 _main ->
                                   let k10 q9 pending9 tick9 res4 =
                                     let k9 q8 pending8 tick8 res3 =
                                       k8 q8 pending8 tick8 res3 in 
                                     res4 k9 q9 pending9 tick9 prefi0 in 
                                   _main k10 q7 pending7 tick7 prefnpool in 
                        let f3 = fun k11 q10 pending10 tick10 npool ->
                                   let f4 = fun k12 q11 pending11 tick11 i0 ->
                                              let k15 q14 pending14 tick14 res7 =
                                                let k14 q13 pending13 tick13 res6 =
                                                  let x1 = 0 in 
                                                  let k13 q12 pending12 tick12 res5 =
                                                    k12 q12 pending12 tick12 res5 in 
                                                  res6 k13 q13 pending13 tick13 x1 in 
                                                res7 k14 q14 pending14 tick14 npool in 
                                              listener k15 q11 pending11 tick11 i0 in 
                                   k11 q10 pending10 tick10 f4 in 
                        let k7 q6 pending6 tick6 res2 =
                          k6 q6 pending6 tick6 res2 in 
                        f2 k7 q5 pending5 tick5 f3 in 
             let rec listener k16 q15 pending15 tick15 i =
               let f5 = fun k17 q16 pending16 tick16 npool ->
                          let f6 = fun k18 q17 pending17 tick17 pend ->
                                     let k22 q21 pending21 tick21 res11 =
                                       let x4 = 0 in 
                                       let x3 = res11 = x4 in 
                                       let x5 = pend < npool in 
                                       let x2 = x3 && x5 in 
                                       let k19 q18 pending18 tick18 res8 =
                                         k18 q18 pending18 tick18 res8 in 
                                       let k20 q19 pending19 tick19 res9 =
                                         let x21 = 1 in 
                                         let x20 = () in 
                                         let k34 q33 pending33 tick33 x19 =
                                           let k37 q36 pending36 tick36 res23 =
                                             let k36 q35 pending35 tick35 res22 =
                                               let x23 = 1 in 
                                               let x22 = pend + x23 in 
                                               let k35 q34 pending34 tick34 res21 =
                                                 let x18 = x20 ; res21 in  k19 q34 pending34 tick34 x18 in 
                                               res22 k35 q35 pending35 tick35 x22 in 
                                             res23 k36 q36 pending36 tick36 npool in 
                                           listener k37 q33 pending33 tick33 i in 
                                         ev_assert k34 q19 pending19 tick19 x21 in 
                                       let k21 q20 pending20 tick20 res10 =
                                         let x7 = 0 in 
                                         let x6 = pend > x7 in 
                                         let k23 q22 pending22 tick22 res12 =
                                           k19 q22 pending22 tick22 res12 in 
                                         let k24 q23 pending23 tick23 res13 =
                                           let x15 = 2 in 
                                           let x14 = () in 
                                           let k30 q29 pending29 tick29 x13 =
                                             let k33 q32 pending32 tick32 res20 =
                                               let k32 q31 pending31 tick31 res19 =
                                                 let x17 = 1 in 
                                                 let x16 = pend - x17 in 
                                                 let k31 q30 pending30 tick30 res18 =
                                                   let x12 = x14 ; res18 in  k23 q30 pending30 tick30 x12 in 
                                                 res19 k31 q31 pending31 tick31 x16 in 
                                               res20 k32 q32 pending32 tick32 npool in 
                                             listener k33 q29 pending29 tick29 i in 
                                           ev_assert k30 q23 pending23 tick23 x15 in 
                                         let k25 q24 pending24 tick24 res14 =
                                           let x11 = 3 in 
                                           let x10 = () in 
                                           let k26 q25 pending25 tick25 x9 =
                                             let k29 q28 pending28 tick28 res17 =
                                               let k28 q27 pending27 tick27 res16 =
                                                 let k27 q26 pending26 tick26 res15 =
                                                   let x8 = x10 ; res15 in  k23 q26 pending26 tick26 x8 in 
                                                 res16 k27 q27 pending27 tick27 pend in 
                                               res17 k28 q28 pending28 tick28 npool in 
                                             listener k29 q25 pending25 tick25 i in 
                                           ev_assert k26 q24 pending24 tick24 x11 in 
                                         if x6 then k24 q20 pending20 tick20 x6 else k25 q20 pending20 tick20 x6 in 
                                       if x2 then k20 q21 pending21 tick21 x2 else k21 q21 pending21 tick21 x2 in 
                                     nondet k22 q17 pending17 tick17 i in 
                          k17 q16 pending16 tick16 f6 in 
               k16 q15 pending15 tick15 f5 in 
             let k5 q4 pending4 tick4 res1 =
               k4 q4 pending4 tick4 res1 in 
             f1 k5 q3 pending3 tick3 listener in 
  let f7 = fun k38 q37 pending37 tick37 i ->
             let x27 = i * i in  let x26 = x27 * i in  let x28 = 199 in 
                                                       let x25 = x26 * x28 in  let x29 = 2 in 
                                                                               let x24 = x25 mod x29 in  k38 q37 pending37 tick37 x24 in 
  let k3 q2 pending2 tick2 res0 =
    res0 in 
  f0 k3 q1 pending1 tick1 f7