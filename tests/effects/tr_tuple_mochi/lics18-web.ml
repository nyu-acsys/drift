(*
Tuple Encoding of Product Program.

Source Program: 

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

let ev_step0 evx cfg0 =
  (match cfg0 with 
   (q,acc0) -> (match acc0 with 
                (pending,tick) -> if (((q = 0) && (tick < 42)) && (evx = 1)) then (0,((pending + 1),(tick + 1)))
                                  else
                                    if (((q = 0) && (tick < 42)) && (evx = 2)) then (0,((pending - 1),(tick + 1)))
                                    else
                                      if (((q = 0) && (tick < 42)) && (evx = 3)) then (0,(pending,(tick + 1)))
                                      else
                                        if (((q = 0) && (tick = 42)) && (evx = 1)) then (42,((pending + 1),43))
                                        else
                                          if (((q = 0) && (tick = 42)) && (evx = 2)) then (42,((pending - 1),43))
                                          else
                                            if (((q = 0) && (tick = 42)) && (evx = 3)) then (42,(pending,43))
                                            else
                                              if (((q = 42) && (evx = 2)) && (pending = 1)) then (0,(0,0))
                                              else
                                                if ((q = 42) && (evx = 2)) then (42,((pending - 1),(tick - 1)))
                                                else if (q = 42) then (666,(0,0))
                                                     else if (q = 666) then (666,(0,0))
                                                          else (q,(pending,tick))))


let ev_step_asst0 cfg1 =
  (match cfg1 with 
   (q,acc1) -> (match acc1 with 
                (pending,tick) -> assert ((q = 666) || (pending <= tick))))


let rec listener i cfg2 =
  ((fun npool cfg3 ->
      ((fun pend cfg4 ->
          if
            ((Random.int(0) >= 0) && (pend < npool))
            then
            (match ((fun cfg15 ->
                       ((ev_step_asst0 cfg15) ; ((),cfg15))) ((ev_step0 1) cfg4)) with 
             (x8,cfg16) -> (match (match (match ((listener i) cfg16) with 
                                          (x10,cfg18) -> ((x10 npool) cfg18)) with  (x11,cfg19) -> ((x11 (pend + 1)) cfg19)) with 
                                   (x9,cfg17) -> ((x8 ; x9),cfg17)))
               else
                 if
                   (pend > 0)
                   then
                   (match ((fun cfg10 ->
                              ((ev_step_asst0 cfg10) ; ((),cfg10))) ((ev_step0 2) cfg4)) with 
                    (x4,cfg11) -> (match (match (match ((listener i) cfg11) with 
                                                 (x6,cfg13) -> ((x6 npool) cfg13)) with  (x7,cfg14) -> ((x7 (pend - 1)) cfg14)) with 
                                          (x5,cfg12) -> ((x4 ; x5),cfg12)))
                      else
                        (match ((fun cfg5 ->
                                   ((ev_step_asst0 cfg5) ; ((),cfg5))) ((ev_step0 3) cfg4)) with 
                         (x0,cfg6) -> (match (match (match ((listener i) cfg6) with 
                                                     (x2,cfg8) -> ((x2 npool) cfg8)) with  (x3,cfg9) -> ((x3 pend) cfg9)) with 
                                              (x1,cfg7) -> ((x0 ; x1),cfg7)))),cfg3)),cfg2) 


let main (npool:int(*-:{cur_v:Int | true = true}*)) (i0:int(*-:{cur_v:Int | true = true}*)) =
  (match (match ((listener i0) (0,(0,0))) with 
          (x12,cfg20) -> ((x12 npool) cfg20)) with  (x13,cfg21) -> ((x13 0) cfg21))
