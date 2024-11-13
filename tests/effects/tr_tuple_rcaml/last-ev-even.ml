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

let ev_step0 evx cfg0 = (match cfg0 with 
                         (q,acc) -> (q,evx))


let asst_final0 cfg1 = (match cfg1 with 
                        (q,acc) -> assert ((acc mod 2) = 0))


let rec foo vv cfg2 = ((fun f cfg3 ->
                          if (vv = 0) then ((() ; 0),((ev_step0 f) cfg3))
                          else (match (match ((foo (vv - 1)) ((ev_step0 vv) cfg3)) with 
                                       (x0,cfg4) -> ((x0 f) cfg4)) with  (x1,cfg5) -> ((() ; x1),cfg5))),cfg2) 


let main (v:int(*-:{cur_v:Int | true = true}*)) (final:int(*-:{cur_v:Int | true = true}*)) = (match if
                                                                                                      ((final mod 2) = 0)
                                                                                                      then
                                                                                                      (match ((foo v) (0,0)) with 
                                                                                                       (x4,cfg8) -> ((x4 final) cfg8))
                                                                                                      else
                                                                                                        (match ((foo v) (0,0)) with 
                                                                                                         (x2,cfg6) -> (match if (v >= 0) then ((final + 1),cfg6)
                                                                                                                             else (final,cfg6) with 
                                                                                                                       (x3,cfg7) -> ((x2 x3) cfg7))) with 
                                                                                                      (e0,acfg0) -> ((asst_final0 acfg0) ; e0))

[@@@assert "typeof(main) <: int -> int -> int"]
