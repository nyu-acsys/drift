(*
Tuple Encoding of Product Program.

Source Program: 

let rec f x pos neg = 
  if x mod 2 = 0 then 
    ev pos
  else
    ev neg;
  if (x <= 0) then 0 else f (x-2) (pos+1) (neg-1)


let main (v:int(*-:{v:Int | true}*)) (p:int(*-:{v:Int | true}*)) (n:int(*-:{v:Int | true}*)) =
  if p > 0 && n < 0 then
    f v p n
  else 0 
  


Property: 

(* Disjunctive *)

QSet   = [0;1;2;3]; 

delta  = fun evx (q, acc) -> 
       	 if ((q = 0) && (evx > 0)) then (1, evx)
    else if ((q = 0) && (evx < 0)) then (2, evx)
    (* q1=heating *)
    else if ((q = 1) && (evx > acc)) then (1, evx)
    (* q2=cooling *)
    else if ((q = 2) && (evx < acc)) then (2, evx)
    else (3, evx);

IniCfg = (0, 0);

(* if event is pos, x must  be even ; *)
assert = fun (q, acc) -> (q < 3);



*)

let ev_step0 evx cfg0 =
  (match cfg0 with 
   (q,acc) -> if ((q = 0) && (evx > 0)) then (1,evx)
              else if ((q = 0) && (evx < 0)) then (2,evx)
                   else if ((q = 1) && (evx > acc)) then (1,evx)
                        else if ((q = 2) && (evx < acc)) then (2,evx)
                             else (3,evx))


let ev_step_asst0 cfg1 =
  (match cfg1 with 
   (q,acc) -> assert (q < 3))


let rec f x cfg2 =
  ((fun pos cfg3 ->
      ((fun neg cfg4 ->
          (match if ((x mod 2) = 0) then ((fun cfg6 ->
                                             ((ev_step_asst0 cfg6) ; ((),cfg6))) ((ev_step0 pos) cfg4))
                 else ((fun cfg5 ->
                          ((ev_step_asst0 cfg5) ; ((),cfg5))) ((ev_step0 neg) cfg4)) with 
           (x0,cfg7) -> (match if (x <= 0) then (0,cfg7)
                               else (match (match ((f (x - 2)) cfg7) with 
                                            (x2,cfg9) -> ((x2 (pos + 1)) cfg9)) with  (x3,cfg10) -> ((x3 (neg - 1)) cfg10)) with 
                               (x1,cfg8) -> ((x0 ; x1),cfg8)))),cfg3)),cfg2) 


let main (v:int(*-:{cur_v:Int | true = true}*)) (p:int(*-:{cur_v:Int | true = true}*)) (n:int(*-:{cur_v:Int | true = true}*)) =
  if ((p > 0) && (n < 0)) then (match (match ((f v) (0,0)) with 
                                       (x4,cfg11) -> ((x4 p) cfg11)) with  (x5,cfg12) -> ((x5 n) cfg12))
                               else (0,(0,0))