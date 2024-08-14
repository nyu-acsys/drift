(*
Tuple Encoding of Product Program.

Source Program: 

let rec apply f x = (ev 1; if x <= 0 then 0 else ((apply f (f x))))

let pred x = x - 1
let succ x = x + 1

let main (n:int(*-:{v:Int | true}*)) =
  let _ = apply pred n in
  ev 2; 0

Property: 

(*
   Traces are just [1;2]
   q0 --1--> q1 --2--> q2
   q3 is the error state
*)
QSet   = [0; 1; 2; 3]; 

delta  = fun evx (q, acc) -> 
  if      ((evx = 1) && (q = 0 || q = 1)) then (1,acc)
  else if ((evx = 2) && (q = 1)) then (2,acc)
  else (3,0);

IniCfg = (0, 0);

assertFinal = fun (q, acc) -> q = 2;


*)

let ev_step0 evx cfg0 = (match cfg0 with 
                         (q,acc) -> if ((evx = 1) && ((q = 0) || (q = 1))) then (1,acc)
                                    else if ((evx = 2) && (q = 1)) then (2,acc)
                                         else (3,0))


let asst_final0 cfg1 = (match cfg1 with 
                        (q,acc) -> assert (q = 2))


let rec apply f cfg2 = ((fun x cfg3 ->
                           (match if (x <= 0) then (0,((ev_step0 1) cfg3))
                                  else (match ((apply f) ((ev_step0 1) cfg3)) with 
                                        (x0,cfg4) -> (match ((f x) cfg4) with 
                                                      (x1,cfg5) -> ((x0 x1) cfg5))) with 
                                  (x2,cfg6) -> ((() ; x2),cfg6))),cfg2)


let pred x cfg7 = ((x - 1),cfg7)


let succ x cfg8 = ((x + 1),cfg8) 


let main (n:int(*-:{cur_v:Int | true = true}*)) = (match let _0 = (match ((apply pred) (0,0)) with 
                                                                   (x3,cfg10) -> ((x3 n) cfg10)) in  (match _0 with 
                                                                                                      (_,cfg9) -> ((() ; 0),((ev_step0 2) cfg9))) with 
                                                         (e0,acfg0) -> ((asst_final0 acfg0) ; e0))