(*
Tuple Encoding of Product Program.

Source Program: 


let rec mono t x =
  ev t;
  if x <= 0 then 0
  else
    mono (t+x) (x-1) (* increase t by positive amount *)

let main (u: int(*-:{v:Int | true}*)) = 
  if u>0 then mono 1 u else 0



Property: 

(*
   Event values increase [1;2;5;...]
   Save previous value
   q1 is the error state
*)
QSet   = [0;1]; 

delta  = fun evx (q, acc) -> 
  if   (q = 0 && evx >= acc) then (0, evx)
  else (1, 0);

IniCfg = (0, 0);

assertFinal = fun (q, acc) -> q = 0;


*)

let ev_step0 evx cfg0 =
  (match cfg0 with 
   (q,acc) -> (if ((q = 0) && (evx >= acc)) then (0,evx)
              else (1,0)))


let asst_final0 cfg1 =
  (match cfg1 with 
   (q,acc) -> assert (q = 0))


let rec mono t x cfg2 =
  (if (x <= 0) then (0,((ev_step0 t) cfg2))
  else (((mono (t + x)) (x - 1)) ((ev_step0 t) cfg2))) 


let main (u:int(*-:{v:Int | true}*)) = 
  (match (if (u > 0) then (((mono 1) u) (0,0))
         else (0,(0,0))) with 
   (e0,cfg4) -> ((asst_final0 cfg4) ; e0))