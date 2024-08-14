(*
Tuple Encoding of Product Program.

Source Program: 

let rec f x = 
  if x <= 0 then ()
  else begin ev 1; f (x - 1); ev 2 end

let main (n:int(*-:{v:Int | true}*)) = 
  f n


Property: 

(* 
 *** Encoding of the bench Example 2.1 
 *** taken from Kobayashi "Model checking higher order functions"
 ***
 *** the original program was 
 *** S -> F c.
 *** F x -> a x (F (b x))
 ***
 *** property defined by the automata: a^*b^*
*)

QSet  = [0; 1; 2];

delta = fun evx (q, acc) -> 
      if (q = 0) && (evx = 1) then (0, acc) 
      else if (q = 0) && (evx = 1) then (0, acc) 
      else if (q = 0) && (evx = 2) then (1, acc)
      else if (q = 1) && (evx = 2) then (1, acc) 
      else (2, acc);

IniCfg = (0, 0);  

assert = fun (q, acc) -> q < 2;


  	 


*)

let ev_step0 evx cfg0 = (match cfg0 with 
                         (q,acc) -> if ((q = 0) && (evx = 1)) then (0,acc)
                                    else if ((q = 0) && (evx = 1)) then (0,acc)
                                         else if ((q = 0) && (evx = 2)) then (1,acc)
                                              else if ((q = 1) && (evx = 2)) then (1,acc)
                                                   else (2,acc))


let ev_step_asst0 cfg1 = (match cfg1 with 
                          (q,acc) -> assert (q < 2))


let rec f x cfg2 = if (x <= 0) then ((),cfg2)
                   else
                     (match ((fun cfg3 ->
                                ((ev_step_asst0 cfg3) ; ((),cfg3))) ((ev_step0 1) cfg2)) with 
                      (x0,cfg4) -> (match (match ((f (x - 1)) cfg4) with 
                                           (x2,cfg6) -> (match ((fun cfg8 ->
                                                                   ((ev_step_asst0 cfg8) ; ((),cfg8))) ((ev_step0 2) cfg6)) with 
                                                         (x3,cfg7) -> ((x2 ; x3),cfg7))) with 
                                           (x1,cfg5) -> ((x0 ; x1),cfg5))) 


let main (n:int(*-:{cur_v:Int | true = true}*)) = ((f n) (0,0))