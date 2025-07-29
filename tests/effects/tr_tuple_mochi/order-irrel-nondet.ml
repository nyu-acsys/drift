(*
Tuple Encoding of Product Program.

Source Program: 

(* 
there are two particular events (let us treat them as integers
 c and -c for some c) such that at most one of them is permitted 
 to occur during any execution
*)
let rec order d c b = 
  if (d > 0) then begin
     (* begin if ( d mod 2 = 0 ) then ev c else ev(-c) end; *)
     ev (if b = 1 then c else (-c)); 
     order (d - 2) c b
  end else 0

let main (dd:int(*-:{v:Int | true}*)) (cc:int(*-:{v:Int | v > 0}*)) = 
  let bb = if (Random.int(0) > 0) then 1 else 0 in
  order dd cc bb



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

let ev_step0 evx cfg0 =
  (match cfg0 with 
   (q,acc) -> (if (q = 1) then (1,acc)
              else (if (acc = 0) then (q,evx)
                   else (if (acc = evx) then (q,acc)
                        else (1,acc)))))


let asst_final0 cfg1 =
  (match cfg1 with 
   (q,acc) -> assert (q = 0))


let rec order d c b cfg2 =
  (if (d > 0) then ((((order (d - 2)) c) b) ((ev_step0 (if (b = 1) then c
                                                       else (- c))) cfg2))
  else (0,cfg2)) 


let main (dd:int(*-:{cur_v:Int | true = true}*)) (cc:int(*-:{cur_v:Int | cur_v = 0}*)) =
if (cc > 0) then begin
  (match let bb = (if (Random.int(0) > 0) then 1
                  else 0) in 
         ((((order dd) cc) bb) (0,0)) with 
   (e0,cfg4) -> ((asst_final0 cfg4) ; e0))
end
else
  0
