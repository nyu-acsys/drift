(*
Tuple Encoding of Product Program.

Source Program: 

(* 
there are two particular events (let us treat them as integers
 c and -c for some c) such that at most one of them is permitted 
 to occur during any execution
*)
let rec order d c = 
  if (d > 0) then begin
     (* begin if ( d mod 2 = 0 ) then ev c else ev(-c) end; *)
     ev (if d mod 2 = 0 then c else (-c)); 
     order (d - 2) c
  end else 0

let main (dd:int(*-:{v:Int | true}*)) (cc:int(*-:{v:Int | v > 0}*)) = 
  order dd cc



Property: 

(* there are two particular events (let us treat them as integers
 c and -c for some c) such that at most one of them is permitted 
 to occur during any execution *)

QSet   = [0;1]; 

delta  = fun evx (q, acc) -> 
    if q = 1 then (1, acc)
    else if acc = 0 then (q, evx) 
    else if acc = evx then (q, acc)
    else (1,acc);

IniCfg = (0, 0);

assertFinal = fun (q, acc) -> q = 0;


*)

let ev_step0 evx cfg0 = (match cfg0 with 
                         (q,acc) -> if (q = 1) then (1,acc)
                                    else if (acc = 0) then (q,evx)
                                         else if (acc = evx) then (q,acc)
                                              else (1,acc))


let asst_final0 cfg1 = (match cfg1 with 
                        (q,acc) -> assert (q = 0))


let rec order d cfg2 = ((fun c cfg3 ->
                           if
                             (d > 0)
                             then
                             (match (match if ((d mod 2) = 0) then (c,cfg3)
                                           else ((- c),cfg3) with 
                                     (x0,cfg4) -> ((),((ev_step0 x0) cfg4))) with 
                                    (x1,cfg5) -> (match (match ((order (d - 2)) cfg5) with 
                                                         (x3,cfg7) -> ((x3 c) cfg7)) with  (x2,cfg6) -> ((x1 ; x2),cfg6)))
                                    else (0,cfg3)),cfg2) 


let main (dd:int(*-:{cur_v:Int | true = true}*)) (cc:int(*-:{cur_v:Int | cur_v = 0}*)) = 
  if cc > 0 then
  (match (match ((order dd) (0,0)) with 
     (x4,cfg8) -> ((x4 cc) cfg8)) with 
    (e0,acfg0) -> ((asst_final0 acfg0) ; e0))
  else 0

[@@@assert "typeof(main) <: int -> int -> int"]
