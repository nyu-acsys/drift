(*
Tuple Encoding of Product Program.

Source Program: 

(* 
 there are two particular events
 (let us treat them as integers c and -c for some c)
 and a temporal ordering:
 q0 --(c)--> q1 --(-c)--> q2
 where q0 and q1 are error states
*)

let rec busy n t =
  if n <= 0 then begin 
    ev -t; 
    0
  end else busy (n - 1) t

let main (x:int(*-:{v:Int | true}*)) (n:int(*-:{v:Int | true}*)) = 
  ev x;
  busy n x


(*
if dd > 0 then begin ev dd; ev(-dd) end
  else begin ev(-dd); ev dd; end
*)



Property: 

QSet   = [0;1;2]; 

delta  = fun evx (q, acc) -> 
    if (q = 0) then (1, evx)
    else if (q = 1) && (acc + evx = 0) then (2, acc)
    else if (q = 2) then (2, acc)
    else (q, acc);

IniCfg = (0, 0);

assertFinal = fun (q, acc) -> q = 2;


*)

let ev_step0 evx cfg0 =
  (match cfg0 with 
   (q,acc) -> (if (q = 0) then (1,evx)
              else (if ((q = 1) && ((acc + evx) = 0)) then (2,acc)
                   else (if (q = 2) then (2,acc)
                        else (q,acc)))))


let asst_final0 cfg1 =
  (match cfg1 with 
   (q,acc) -> assert (q = 2))


let rec busy n t cfg2 =
  (if (n <= 0) then ((() ; 0),((ev_step0 (- t)) cfg2))
  else (((busy (n - 1)) t) cfg2)) 


let main (x:int(*-:{cur_v:Int | true = true}*)) (n:int(*-:{cur_v:Int | true = true}*)) =
  (match (((busy n) x) ((ev_step0 x) (0,0))) with 
   (e0,cfg3) -> ((asst_final0 cfg3) ; e0))

[@@@assert "typeof(main) <: int -> int -> int"]
