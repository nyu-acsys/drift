(*
Tuple Encoding of Product Program.

Source Program: 

let next x = 
  ev x;
  x - 1 

let rec loop s = 
  if (s > 0) then loop (next s)
  else ()

let main (u: unit(*-:{v:Unit | unit}*)) = 
  ev 0;
  loop 4



Property: 

(* ============================================================================================ *)
(* ===                         III. 3-State AUTOMATON & Initial CFG                         === *)
(* ============================================================================================ *)

QSet   = [0; 1; 2; 3]; 

delta  = fun evx (q, (max, min)) -> 
       if evx < min then (q, (max,evx)) 
	   else if evx > max then (q, (evx,min)) 
	   else (q, (max,min)); 

IniCfg = (0, (0, 0));

assertFinal = fun (q, (max, min)) -> max + min = 0;



*)

let ev_step0 evx cfg0 = (match cfg0 with 
                         (q,acc0) -> (match acc0 with 
                                      (max,min) -> if (evx < min) then (q,(max,evx))
                                                   else if (evx > max) then (q,(evx,min))
                                                        else (q,(max,min))))


let asst_final0 cfg1 = (match cfg1 with 
                        (q,acc1) -> (match acc1 with 
                                     (max,min) -> assert ((max + min) = 0)))


let next x cfg2 = ((() ; (x - 1)),((ev_step0 x) cfg2))


let rec loop s cfg3 = if (s > 0) then (match ((next s) cfg3) with 
                                       (x0,cfg4) -> ((loop x0) cfg4)) else ((),cfg3) 


let main (u:unit(*-:{cur_v:Unit | unit = unit}*)) = (match (match ((loop 4) ((ev_step0 0) (0,(0,0)))) with 
                                                            (x1,cfg5) -> ((() ; x1),cfg5)) with  (e0,acfg0) -> ((asst_final0 acfg0) ; e0))