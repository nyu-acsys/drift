(*
Tuple Encoding of Product Program.

Source Program: 

let rec sum x = 
  ev x;
  if x <= 0 then 0 else sum (x - 2)

let main (v:int(*-:{v:Int | true}*)) =
  if v mod 2 = 0 then sum v else sum (v + 1)


Property: 

(* Accumulate the Sum, and Sum is Even *)

QSet   = [0]; 

delta  = fun evx (q, acc) -> (q,acc + evx);

IniCfg = (0, 0);

assert = fun (q, acc) -> true;
 
assertFinal = fun (q, acc) -> (acc mod 2 = 0);



*)

let ev_step0 evx cfg0 =
  (match cfg0 with 
   (q,acc) -> (q,(acc + evx)))


let ev_step_asst0 cfg1 =
  (match cfg1 with 
   (q,acc) -> assert true)


let asst_final0 cfg2 =
  (match cfg2 with 
   (q,acc) -> assert ((acc mod 2) = 0))


let rec sum x cfg3 =
  (match let cfg4 = ((ev_step0 x) cfg3) in 
         ((ev_step_asst0 cfg4),cfg4) with 
   (x0,cfg5) -> (if (x <= 0) then (0,cfg5)
                else ((sum (x - 2)) cfg5))) 


let main (v:int(*-:{cur_v:Int | true = true}*)) =
  (match (if ((v mod 2) = 0) then ((sum v) (0,0))
         else ((sum (v + 1)) (0,0))) with 
   (e0,cfg7) -> ((asst_final0 cfg7) ; e0))

[@@@assert "typeof(main) <: int -> int"]
