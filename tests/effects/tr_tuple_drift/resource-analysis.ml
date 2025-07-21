(*
Tuple Encoding of Product Program.

Source Program: 

let rec spend n =
  ev -1;
  if n <= 0 then 0 else spend (n-1)

let main (gas:int(*-:{v:Int | true}*)) (n:int(*-:{v:Int | true}*)) = 
  if (gas > n && n >= 0) then begin ev gas; spend n end else 0


Property: 

(* Resource Analysis *)

QSet   = [0]; 

delta  = fun evx (q, acc) -> (q,acc + evx);

IniCfg = (0, 0);

assert = fun (q, acc) -> (acc >= 0);


*)

let ev_step0 evx cfg0 =
  (match cfg0 with 
   (q,acc) -> (q,(acc + evx)))


let ev_step_asst0 cfg1 =
  (match cfg1 with 
   (q,acc) -> assert (acc >= 0))


let rec spend n cfg2 =
  (match let cfg3 = ((ev_step0 (-1)) cfg2) in 
         ((ev_step_asst0 cfg3),cfg3) with 
   (x0,cfg4) -> (if (n <= 0) then (0,cfg4)
                else ((spend (n - 1)) cfg4))) 


let main (gas:int(*-:{cur_v:Int | true = true}*)) (n:int(*-:{cur_v:Int | true = true}*)) =
  (if
     ((gas > n) && (n >= 0))
     then
     (match let cfg6 = ((ev_step0 gas) (0,0)) in 
            ((ev_step_asst0 cfg6),cfg6) with 
      (x2,cfg7) -> ((spend n) cfg7))
     else (0,(0,0)))