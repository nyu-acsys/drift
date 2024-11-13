(*
Tuple Encoding of Product Program.

Source Program: 

let rec f x b pos neg = 
  if b = 0 then 
    ev pos
  else
    ev neg;
  if (x <= 0) then 0 else f (x-1) b pos neg


let main (v:int(*-:{v:Int | true}*)) (p:int(*-:{v:Int | true}*)) (n:int(*-:{v:Int | true}*)) =
  if p > 0 && n < 0 then
    let bb = if nondet then 1 else 0 in
    f v bb p n
  else 0 
  


Property: 

(* Disjunctive *)

QSet   = [0;1;2;3]; 

delta  = fun evx (q, acc) -> 
       	     if ((q = 0) || (q = 1)) && (evx > 0) then (1,acc)
	     else if (q = 1) && (evx < 0) then (3, acc)
       	     else if ((q = 0) || (q = 2)) && (evx < 0) then (2,acc)
	     else if (q = 2) && (evx > 0) then (3, acc)
	     else (3, acc);

IniCfg = (0, 0);

(* if event is pos, x must  be even ; *)
assert = fun (q, acc) -> (q < 3);



*)

let ev_step0 evx cfg0 =
  (match cfg0 with 
   (q,acc) -> if (((q = 0) || (q = 1)) && (evx > 0)) then (1,acc)
              else if ((q = 1) && (evx < 0)) then (3,acc)
                   else if (((q = 0) || (q = 2)) && (evx < 0)) then (2,acc)
                        else if ((q = 2) && (evx > 0)) then (3,acc)
                             else (3,acc))


let ev_step_asst0 cfg1 =
  (match cfg1 with 
   (q,acc) -> assert (q < 3))


let rec f x cfg2 =
  ((fun b cfg3 ->
      ((fun pos cfg4 ->
          ((fun neg cfg5 ->
              (match if (b = 0) then ((fun cfg7 ->
                                         ((ev_step_asst0 cfg7) ; ((),cfg7))) ((ev_step0 pos) cfg5))
                     else ((fun cfg6 ->
                              ((ev_step_asst0 cfg6) ; ((),cfg6))) ((ev_step0 neg) cfg5)) with 
               (x0,cfg8) -> (match if (x <= 0) then (0,cfg8)
                                   else
                                     (match (match (match ((f (x - 1)) cfg8) with 
                                                    (x2,cfg10) -> ((x2 b) cfg10)) with  (x3,cfg11) -> ((x3 pos) cfg11)) with 
                                             (x4,cfg12) -> ((x4 neg) cfg12)) with 
                                     (x1,cfg9) -> ((x0 ; x1),cfg9)))),cfg4)),cfg3)),cfg2) 


let main (v:int(*-:{cur_v:Int | true = true}*)) (p:int(*-:{cur_v:Int | true = true}*)) (n:int(*-:{cur_v:Int | true = true}*)) =
  if
    ((p > 0) && (n < 0))
    then
    let bb0 = if (nondet_op) then (1,(0,0))
              else (0,(0,0)) in 
    (match bb0 with 
     (bb,cfg13) -> (match (match (match ((f v) cfg13) with 
                                  (x5,cfg14) -> ((x5 bb) cfg14)) with  (x6,cfg15) -> ((x6 p) cfg15)) with 
                           (x7,cfg16) -> ((x7 n) cfg16))) else (0,(0,0))