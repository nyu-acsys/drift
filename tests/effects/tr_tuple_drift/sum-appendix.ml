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
  (match let cfg4 = ((ev_step0 1) cfg3) in 
         ((ev_step_asst0 cfg4),cfg4) with 
   (x0,cfg5) -> ((x0 ; (fun y ->
                          (x + y))),cfg5)) 


let main (u: unit(*-:{v:Unit | unit}*)) = 
  (match (match let cfg7 = ((ev_step0 2) (0,0)) in 
                ((ev_step_asst0 cfg7),cfg7) with 
          (x1,cfg8) -> (match (match (match (match let cfg9 = ((ev_step0 3) cfg8) in 
                                                   ((ev_step_asst0 cfg9),cfg9) with 
                                             (x2,cfg10) -> ((x2 ; 10),cfg10)) with 
                                            (x3,cfg11) -> ((sum x3) cfg11)) with 
                                      (x4,cfg12) -> (match (match let cfg13 = ((ev_step0 4) cfg12) in 
                                                                  ((ev_step_asst0 cfg13),cfg13) with 
                                                            (x5,cfg14) -> ((x5 ; 20),cfg14)) with 
                                                           (x6,cfg15) -> ((x4 x6),cfg15))) with 
                                        (x7,cfg16) -> let cfg17 = ((ev_step0 x7) cfg16) in 
                                                      ((ev_step_asst0 cfg17),cfg17))) with 
                              (e0,cfg6) -> ((asst_final0 cfg6) ; e0))