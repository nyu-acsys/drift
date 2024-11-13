(*
Tuple Encoding of Product Program.

Source Program: 




let main (v:int(*-:{v:Int | true}*)) =
  if v = 0 then begin ev 1; 0 end else 0


Property: 

(* Dependent *)

QSet   = [0]; 

delta  = fun evx (q, acc) -> 
   if evx > prefv then (q,acc + evx) else (q, acc);

IniCfg = (0, 0);

assert = fun (q, acc) -> (acc >= 0);



*)

let main (prefv:int) =

let ev_step0 evx cfg0 =
  (match cfg0 with 
   (q,acc) -> if (evx > prefv) then (q,(acc + evx))
              else (q,acc))
in

let ev_step_asst0 cfg1 =
  (match cfg1 with 
   (q,acc) -> assert (acc >= 0)) 
in


if (prefv = 0) then (match ((fun cfg2 ->
                             ((ev_step_asst0 cfg2) ; ((),cfg2))) ((ev_step0 1) (0,0))) with 
                   (x0,cfg3) -> (match (0,cfg3) with 
                                 (x1,cfg4) -> ((x0 ; x1),cfg4)))
                  else (0,(0,0))
