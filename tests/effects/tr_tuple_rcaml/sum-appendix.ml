(*
Tuple Encoding of Product Program.

Source Program: 


let sum x = 
  ev 1; (fun y -> x + y)

let main (u: unit(*-:{v:Unit | unit}*)) = 
  ev 2;
  ev (sum (ev 3; 10) (ev 4; 20))


Property: 

QSet   = [0]; 

delta  = fun evx (q, acc) -> (q, (evx + acc));

IniCfg = (0, 0);

assert = fun (q, acc) -> acc >= 0;

assertFinal = fun (q, acc) -> acc = 40;


*)

let ev_step0 evx cfg0 =
  (match cfg0 with 
   (q,acc) -> (q,(evx + acc)))


let ev_step_asst0 cfg1 =
  (match cfg1 with 
   (q,acc) -> assert (acc >= 0))


let asst_final0 cfg2 =
  (match cfg2 with 
   (q,acc) -> assert (acc = 40))


let sum x cfg3 =
  (match ((fun cfg4 ->
             ((ev_step_asst0 cfg4) ; ((),cfg4))) ((ev_step0 1) cfg3)) with 
   (x0,cfg5) -> (match ((fun y cfg7 ->
                           ((x + y),cfg7)),cfg5) with 
                 (x1,cfg6) -> ((x0 ; x1),cfg6))) 


let main (u:unit(*-:{cur_v:Unit | unit = unit}*)) =
  (match (match ((fun cfg8 ->
                    ((ev_step_asst0 cfg8) ; ((),cfg8))) ((ev_step0 2) (0,0))) with 
          (x2,cfg9) -> (match (match (match (match (match ((fun cfg11 ->
                                                              ((ev_step_asst0 cfg11) ; ((),cfg11))) ((ev_step0 3) cfg9)) with 
                                                    (x4,cfg12) -> (match (10,cfg12) with 
                                                                   (x5,cfg13) -> ((x4 ; x5),cfg13))) with 
                                                    (x6,cfg14) -> ((sum x6) cfg14)) with 
                                                   (x7,cfg15) -> (match (match ((fun cfg16 ->
                                                                                   ((ev_step_asst0 cfg16) ; ((),cfg16))) ((ev_step0 4) cfg15)) with 
                                                                         (x8,cfg17) -> (match (20,cfg17) with 
                                                                                        (x9,cfg18) -> ((x8 ; x9),cfg18))) with 
                                                                         (x10,cfg19) -> ((x7 x10) cfg19))) with 
                                                   (x11,cfg21) -> ((fun cfg20 ->
                                                                      ((ev_step_asst0 cfg20) ; ((),cfg20))) ((ev_step0 x11) cfg21))) with 
                                                     (x3,cfg10) -> ((x2 ; x3),cfg10))) with 
                                      (e0,acfg0) -> ((asst_final0 acfg0) ; e0))

[@@@assert "typeof(main) <: unit -> unit"]
