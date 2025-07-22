(*
Tuple Encoding of Product Program.

Source Program: 

let sell n1 = ev 2; ev n1; ()

let buy n2 = ev 1; ev n2; ()

let activity f n3 = f n3; ev 0; ()

let rec repeat n4 = 
  if (n4<=0) then 0 
  else 
    begin
      activity buy 1; 
      activity sell 1; 
      1 + repeat (n4-1)
    end

let main (budget:int(*-:{v:Int | v >= 0}*)) (stock:int(*-:{v:Int | v >= 0}*)) (n:int(*-:{v:Int | v > 0}*)) =
  if (stock >= budget && budget >= n) then
    begin
      activity sell (-stock);
      activity buy (-budget);
      repeat n
    end
  else
    0


Property: 

QSet   = [0;1;2;3]; 

(* [stable state, consumer state, producer state, error state] *)

delta  = fun evx (q, (budget, stock)) -> 
    if (q=0 && evx > 0 && evx < 3) then (evx, (budget, stock))
    else if (q=1 && evx<>0) then (q, ((budget-evx), stock))
    else if (q=2 && evx<>0) then (q, (budget, (stock-evx)))
    else if ((q=1||q=2) && evx=0) then (0, (budget, stock))
    else (3, (budget, stock));

IniCfg = (0, (0,0));

assert = fun (q, (budget, stock)) -> q < 3 && budget >=0 && stock >=0 && stock >= budget;

*)

let ev_step0 evx cfg0 =
  (match cfg0 with 
   (q,acc0) -> (match acc0 with 
                (budget,stock) -> if (((q = 0) && (evx > 0)) && (evx < 3)) then (evx,(budget,stock))
                                  else
                                    if ((q = 1) && (evx <> 0)) then (q,((budget - evx),stock))
                                    else if ((q = 2) && (evx <> 0)) then (q,(budget,(stock - evx)))
                                         else if (((q = 1) || (q = 2)) && (evx = 0)) then (0,(budget,stock))
                                              else (3,(budget,stock))))


let ev_step_asst0 cfg1 =
  (match cfg1 with 
   (q,acc1) -> (match acc1 with 
                (budget,stock) -> assert ((((q < 3) && (budget >= 0)) && (stock >= 0)) && (stock >= budget))))


let sell n1 cfg2 =
  (match ((fun cfg3 ->
             ((ev_step_asst0 cfg3) ; ((),cfg3))) ((ev_step0 2) cfg2)) with 
   (x0,cfg4) -> (match (match ((fun cfg6 ->
                                  ((ev_step_asst0 cfg6) ; ((),cfg6))) ((ev_step0 n1) cfg4)) with 
                        (x2,cfg7) -> (match ((),cfg7) with 
                                      (x3,cfg8) -> ((x2 ; x3),cfg8))) with 
                        (x1,cfg5) -> ((x0 ; x1),cfg5)))


let buy n2 cfg9 =
  (match ((fun cfg10 ->
             ((ev_step_asst0 cfg10) ; ((),cfg10))) ((ev_step0 1) cfg9)) with 
   (x4,cfg11) -> (match (match ((fun cfg13 ->
                                   ((ev_step_asst0 cfg13) ; ((),cfg13))) ((ev_step0 n2) cfg11)) with 
                         (x6,cfg14) -> (match ((),cfg14) with 
                                        (x7,cfg15) -> ((x6 ; x7),cfg15))) with 
                         (x5,cfg12) -> ((x4 ; x5),cfg12)))


let activity f cfg16 =
  ((fun n3 cfg17 ->
      (match ((f n3) cfg17) with 
       (x8,cfg18) -> (match (match ((fun cfg20 ->
                                       ((ev_step_asst0 cfg20) ; ((),cfg20))) ((ev_step0 0) cfg18)) with 
                             (x10,cfg21) -> (match ((),cfg21) with 
                                             (x11,cfg22) -> ((x10 ; x11),cfg22))) with 
                             (x9,cfg19) -> ((x8 ; x9),cfg19)))),cfg16)


let rec repeat n4 cfg23 =
  if (n4 <= 0) then (0,cfg23)
  else
    (match (match ((activity buy) cfg23) with 
            (x12,cfg24) -> ((x12 1) cfg24)) with 
           (x13,cfg25) -> (match (match (match ((activity sell) cfg25) with 
                                         (x15,cfg27) -> ((x15 1) cfg27)) with 
                                        (x16,cfg28) -> (match (match ((repeat (n4 - 1)) cfg28) with 
                                                               (x18,cfg30) -> ((1 + x18),cfg30)) with  (x17,cfg29) -> ((x16 ; x17),cfg29))) with 
                                          (x14,cfg26) -> ((x13 ; x14),cfg26))) 


let main (budget:int(*-:{cur_v:Int | cur_v = 0}*)) (stock:int(*-:{cur_v:Int | cur_v = 0}*)) (n:int(*-:{cur_v:Int | cur_v = 0}*)) =
  if
    ((budget >= 0) && (stock >= 0) && (n > 0) && (stock >= budget) && (budget >= n))
    then
    (match (match ((activity sell) (0,(0,0))) with 
            (x19,cfg31) -> ((x19 (- stock)) cfg31)) with 
           (x20,cfg32) -> (match (match (match ((activity buy) cfg32) with 
                                         (x22,cfg34) -> ((x22 (- budget)) cfg34)) with 
                                        (x23,cfg35) -> (match ((repeat n) cfg35) with 
                                                        (x24,cfg36) -> ((x23 ; x24),cfg36))) with 
                                        (x21,cfg33) -> ((x20 ; x21),cfg33)))
           else (0,(0,(0,0)))

let _ = main 1 1 1
