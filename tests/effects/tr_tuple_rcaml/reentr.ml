(*
Tuple Encoding of Product Program.

Source Program: 

let rec reent d = 
  ev 1; (* acquire *) 
  if (d > 0) then 
    begin
      if (nondet_bool ()) then begin reent (d-1); ev (-1) (* release *) end
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

external nondet_bool : unit -> bool = "unknown"

let ev_step0 evx cfg0 =
  (match cfg0 with 
   (q,acc) -> (if ((q = 0) && ((acc + evx) >= 0)) then (q,(acc + evx))
              else (if ((q = 0) && ((acc + evx) < 0)) then (1,(acc + evx))
                   else (q,acc))))


let ev_step_asst0 cfg1 =
  (match cfg1 with 
   (q,acc) -> assert (q = 0))


let rec reent d cfg2 =
  (match let cfg3 = ((ev_step0 1) cfg2) in 
         ((ev_step_asst0 cfg3),cfg3) with 
   (x0,cfg4) -> (if
                   (d > 0)
                   then
                   (if
                      (nondet_bool ())
                      then
                      (match ((reent (d - 1)) cfg4) with 
                       (x1,cfg5) -> let cfg6 = ((ev_step0 (-1)) cfg5) in 
                                    ((ev_step_asst0 cfg6),cfg6))
                      else ((),cfg4))
                   else ((),cfg4))) 


let main (n:int(*-:{cur_v:Int | cur_v = 0}*)) =
  (match ((reent n) (0,0)) with 
   (x4,cfg9) -> let cfg10 = ((ev_step0 (-1)) cfg9) in 
                ((ev_step_asst0 cfg10),cfg10))

[@@@assert "typeof(main) <: int -> unit * (int * int)"]
