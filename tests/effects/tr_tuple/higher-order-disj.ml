(*
Tuple Encoding of Product Program.

Source Program: 

let rec apply f x = if x <= 0 then 0 else apply f ((f x)) 
let tick c t = ev (if t mod 2 = 0 then c else (-c)); t-2 
let main (n: int(*-:{v:Int | v > 0}*)) (x: int(*-:{v:Int | v > 0 }*)) = 
  apply (tick x) n


(* 
let rec apply q acc f x = 
  if x <= 0 then (0, (q, acc))  
  else 
    let (x', (q', acc')) = f q acc x in
    apply q' acc' x'

let tick q acc c t = 
  let (q', acc') = ev_aut_step q acc (if t mod 2 then x else (-c)) in
  assert (q' = 1 || q' = 2); 
  (t-2, (q', acc'))

let main (n:int) (c:int) = 
  let (q, acc) = (0,0) in
  if (n > 0 && c > 0) then
    apply 
*)


Property: 

(* Disjunctive *)

QSet   = [0;1;2;3]; 

delta  = fun evx (q, acc) -> 
       	     if (q = 0) && (evx > 0) then (1,evx)
	     else if (q = 1) && (evx < 0) then (3, evx)
       	     else if (q = 0) && (evx < 0) then (2,evx)
	     else if (q = 2) && (evx > 0) then (3, evx)
	     else if (q < 3) && (acc <> evx) then (3, evx)
	     else (q, acc);

(* delta = fun evx (q, acc) -> 
      if (q = 0 && evx > 0) then 
      else if q = 0 && acc = 0 then (1, evx)
      else if q = 1 && acc = evx then (q, acc)
      else (2, acc); *)

(* delta = fun evx (q, acc) -> 
      if q = 2 then (q, acc)
      else if q = 0 && acc = 0 then (1, evx)
      else if q = 1 && acc = evx then (q, acc)
      else (2, acc); *)

(* delta = fun evx (q, acc) -> 
      if (q = 1) then (1, acc)
      else if acc = 0 then (0, evx)
      else if acc = evx then (q, acc)
      else (1, acc); *)

IniCfg = (0, 0);

(* if event is pos, x must  be even ; *)
assert = fun (q, acc) -> q = 1 || q = 2;



*)

let ev_step0 evx cfg0 = (match cfg0 with 
                         (q,acc) -> if ((q = 0) && (evx > 0)) then (1,evx)
                                    else
                                      if ((q = 1) && (evx < 0)) then (3,evx)
                                      else if ((q = 0) && (evx < 0)) then (2,evx)
                                           else if ((q = 2) && (evx > 0)) then (3,evx)
                                                else if ((q < 3) && (acc != evx)) then (3,evx)
                                                     else (q,acc))


let ev_step_asst0 cfg1 = (match cfg1 with 
                          (q,acc) -> assert ((q = 1) || (q = 2)))


let rec apply f cfg2 = ((fun x cfg3 ->
                           if (x <= 0) then (0,cfg3)
                           else (match ((apply f) cfg3) with 
                                 (x0,cfg4) -> (match ((f x) cfg4) with 
                                               (x1,cfg5) -> ((x0 x1) cfg5)))),cfg2)


let tick c cfg6 = ((fun t cfg7 ->
                      (match (match if ((t mod 2) = 0) then (c,cfg7)
                                    else ((- c),cfg7) with 
                              (x2,cfg9) -> ((fun cfg8 ->
                                               ((ev_step_asst0 cfg8) ; ((),cfg8))) ((ev_step0 x2) cfg9))) with 
                             (x3,cfg10) -> (match ((t - 2),cfg10) with 
                                            (x4,cfg11) -> ((x3 ; x4),cfg11)))),cfg6) 


let main (n:int(*-:{cur_v:Int | cur_v = 0}*)) (x:int(*-:{cur_v:Int | cur_v = 0}*)) = (match (match ((tick x) (0,0)) with 
                                                                                             (x5,cfg12) -> ((apply x5) cfg12)) with  (x6,cfg13) -> ((x6 n) cfg13))