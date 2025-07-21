(*
Tuple Encoding of Product Program.

Source Program: 

(* Adapted from:
   A Fixpoint Logic and Dependent Effects for
   Temporal Property Verification
   Nanjo et al, LICS 2018 *)

let rec listener i npool pend =
  if (nondet) && pend < npool then begin
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
                (pending,tick) -> (if (((q = 0) && (tick < 42)) && (evx = 1)) then (0,((pending + 1),(tick + 1)))
                                  else
                                    (if (((q = 0) && (tick < 42)) && (evx = 2)) then (0,((pending - 1),(tick + 1)))
                                    else
                                      (if (((q = 0) && (tick < 42)) && (evx = 3)) then (0,(pending,(tick + 1)))
                                      else
                                        (if (((q = 0) && (tick = 42)) && (evx = 1)) then (42,((pending + 1),43))
                                        else
                                          (if (((q = 0) && (tick = 42)) && (evx = 2)) then (42,((pending - 1),43))
                                          else
                                            (if (((q = 0) && (tick = 42)) && (evx = 3)) then (42,(pending,43))
                                            else
                                              (if (((q = 42) && (evx = 2)) && (pending = 1)) then (0,(0,0))
                                              else
                                                (if ((q = 42) && (evx = 2)) then (42,((pending - 1),(tick - 1)))
                                                else
                                                  (if (q = 42) then (666,(0,0))
                                                  else (if (q = 666) then (666,(0,0))
                                                       else (q,(pending,tick))))))))))))))


let ev_step_asst0 cfg1 =
  (match cfg1 with 
   (q,acc1) -> (match acc1 with 
                (pending,tick) -> assert ((q = 666) || (pending <= tick))))


let rec listener i npool pend cfg2 =
  (if
     ((nondet) && (pend < npool))
     then
     (match let cfg9 = ((ev_step0 1) cfg2) in 
            ((ev_step_asst0 cfg9),cfg9) with 
      (x4,cfg10) -> ((((listener i) npool) (pend + 1)) cfg10))
     else
       (if
          (pend > 0)
          then
          (match let cfg6 = ((ev_step0 2) cfg2) in 
                 ((ev_step_asst0 cfg6),cfg6) with 
           (x2,cfg7) -> ((((listener i) npool) (pend - 1)) cfg7))
          else
            (match let cfg3 = ((ev_step0 3) cfg2) in 
                   ((ev_step_asst0 cfg3),cfg3) with 
             (x0,cfg4) -> ((((listener i) npool) pend) cfg4)))) 


let main (npool:int(*-:{v:Int | true}*)) (i0:int(*-:{v:Int | true}*)) =
  ((((listener i0) npool) 0) (0,(0,0)))
