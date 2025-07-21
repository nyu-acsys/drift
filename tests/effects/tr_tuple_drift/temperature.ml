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
   (q,acc) -> (if ((q = 0) && (evx > 0)) then (1,evx)
              else
                (if ((q = 0) && (evx < 0)) then (2,evx)
                else
                  (if ((q = 1) && (evx > acc)) then (1,evx)
                  else (if ((q = 2) && (evx < acc)) then (2,evx)
                       else (3,evx))))))


let ev_step_asst0 cfg1 =
  (match cfg1 with 
   (q,acc) -> assert (q < 3))


let rec f x pos neg cfg2 =
  (match (if ((x mod 2) = 0) then let cfg4 = ((ev_step0 pos) cfg2) in 
                                  ((ev_step_asst0 cfg4),cfg4)
         else let cfg3 = ((ev_step0 neg) cfg2) in 
              ((ev_step_asst0 cfg3),cfg3)) with 
   (x0,cfg5) -> (if (x <= 0) then (0,cfg5)
                else ((((f (x - 2)) (pos + 1)) (neg - 1)) cfg5))) 


let main (v:int(*-:{v:Int | true}*)) (p:int(*-:{v:Int | true}*)) (n:int(*-:{v:Int | true}*)) =
  (if ((p > 0) && (n < 0)) then ((((f v) p) n) (0,0))
  else (0,(0,0)))