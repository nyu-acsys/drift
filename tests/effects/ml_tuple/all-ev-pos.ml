(*
Tuple Encoding of Product Program.

Source Program: 

(* *** Based on COAR: **********************************************************
let rec sum x = if x <= 0 then 0 else x + sum (x - 1)
[@@@assert "typeof(sum) <: (x:{ x:int | true }) -> { ret : int | ret > x }"]
[@@@assert "typeof(sum) <: (x:{ x:int | x > 1 }) -> { ret : int | ret > x }"]
[@@@assert "typeof(sum) <: (x:{ x:int | true }) -> { ret : int | ret >= x }"]
****************************************************************************** *)

(* Properties: prop-all-ev-pos.eff *)

let rec sum x = 
  ev x;
  if x <= 0 then 0 else x + (sum (x - 1))

let main (v:int(*-:{v:Int | true}*)) =
  if v < 0 then sum (0 - v) else sum v  


Property: 

(* All Events Are Positive *)

QSet   = [0;1]; 
delta  = fun evx (q, acc) -> 
       if (evx < 0) then (1, acc) else (0, acc);
IniCfg = (0, 0);
assert = fun (q, acc) -> q = 0; 


*)

let ev_step0 evx cfg0 = (match cfg0 with 
                         (q,acc) -> if (evx < 0) then (1,acc)
                                    else (0,acc))


let ev_step_asst0 cfg1 = (match cfg1 with 
                          (q,acc) -> assert (q = 0))


let rec sum x cfg2 = (match ((fun cfg3 ->
                                ((ev_step_asst0 cfg3) ; ((),cfg3))) ((ev_step0 x) cfg2)) with 
                      (x0,cfg4) -> (match if (x <= 0) then (0,cfg4)
                                          else (match ((sum (x - 1)) cfg4) with 
                                                (x2,cfg6) -> ((x + x2),cfg6)) with  (x1,cfg5) -> ((x0 ; x1),cfg5))) 


let main (v:int(*-:{cur_v:Int | true = true}*)) = if (v < 0) then ((sum (0 - v)) (0,0))
                                                  else ((sum v) (0,0))