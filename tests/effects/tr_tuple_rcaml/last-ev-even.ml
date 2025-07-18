(*
Tuple Encoding of Product Program.

Source Program: 

let rec foo vv f =
  if vv = 0 then begin ev f; 0 end
  else begin ev vv; foo (vv-1) f end 

let main (v:int(*-:{v:Int | true}*)) (final:int(*-:{v:Int | true}*)) =
  if (final mod 2 = 0) then 
    foo v final
  else 
    foo v (if v >= 0 then final+1 else final) 
  


Property: 

(* Last event seen is even *)

QSet   = [0]; 

delta  = fun evx (q, acc) -> (q,evx);

IniCfg = (0, 0);

assertFinal = fun (q, acc) -> acc mod 2 = 0;


*)

let ev_step0 evx cfg0 =
  (match cfg0 with 
   (q,acc) -> (q,evx))


let asst_final0 cfg1 =
  (match cfg1 with 
   (q,acc) -> assert ((acc mod 2) = 0))


let rec foo vv f cfg2 =
  (if (vv = 0) then ((() ; 0),((ev_step0 f) cfg2))
  else (((foo (vv - 1)) f) ((ev_step0 vv) cfg2))) 


let main (v:int(*-:{cur_v:Int | true = true}*)) (final:int(*-:{cur_v:Int | true = true}*)) =
  (match (if ((final mod 2) = 0) then (((foo v) final) (0,0))
         else (((foo v) (if (v >= 0) then (final + 1)
                        else final)) (0,0))) with 
   (e0,cfg4) -> ((asst_final0 cfg4) ; e0))

[@@@assert "typeof(main) <: int -> int -> int"]
