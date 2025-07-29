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
                (budget,stock) -> (if (((q = 0) && (evx > 0)) && (evx < 3)) then (evx,(budget,stock))
                                  else
                                    (if ((q = 1) && (evx <> 0)) then (q,((budget - evx),stock))
                                    else
                                      (if ((q = 2) && (evx <> 0)) then (q,(budget,(stock - evx)))
                                      else
                                        (if (((q = 1) || (q = 2)) && (evx = 0)) then (0,(budget,stock))
                                        else (3,(budget,stock))))))))


let ev_step_asst0 cfg1 =
  (match cfg1 with 
   (q,acc1) -> (match acc1 with 
                (budget,stock) -> assert ((((q < 3) && (budget >= 0)) && (stock >= 0)) && (stock >= budget))))


let sell n1 cfg2 =
  (match let cfg3 = ((ev_step0 2) cfg2) in 
         ((ev_step_asst0 cfg3),cfg3) with 
   (x0,cfg4) -> (match let cfg5 = ((ev_step0 n1) cfg4) in 
                       ((ev_step_asst0 cfg5),cfg5) with 
                 (x1,cfg6) -> ((x1 ; ()),cfg6)))


let buy n2 cfg8 =
  (match let cfg9 = ((ev_step0 1) cfg8) in 
         ((ev_step_asst0 cfg9),cfg9) with 
   (x3,cfg10) -> (match let cfg11 = ((ev_step0 n2) cfg10) in 
                        ((ev_step_asst0 cfg11),cfg11) with 
                  (x4,cfg12) -> ((x4 ; ()),cfg12)))


let activity f n3 cfg14 =
  (match ((f n3) cfg14) with 
   (x6,cfg15) -> (match let cfg16 = ((ev_step0 0) cfg15) in 
                        ((ev_step_asst0 cfg16),cfg16) with 
                  (x7,cfg17) -> ((x7 ; ()),cfg17)))


let rec repeat n4 cfg19 =
  (if (n4 <= 0) then (0,cfg19)
  else
    (match (((activity buy) 1) cfg19) with 
     (x9,cfg20) -> (match (((activity sell) 1) cfg20) with 
                    (x10,cfg21) -> (match ((repeat (n4 - 1)) cfg21) with 
                                    (x11,cfg22) -> ((1 + x11),cfg22))))) 


let main (budget:int(*-:{cur_v:Int | cur_v = 0}*)) (stock:int(*-:{cur_v:Int | cur_v = 0}*)) (n:int(*-:{cur_v:Int | cur_v = 0}*)) =
  if (budget >= 0 && stock >= 0 && n >0) then begin
  (if
     ((stock >= budget) && (budget >= n))
     then
     (match (((activity sell) (- stock)) (0,(0,0))) with 
      (x14,cfg25) -> (match (((activity buy) (- budget)) cfg25) with 
                      (x15,cfg26) -> ((repeat n) cfg26)))
     else (0,(0,(0,0))))
    end
  else
    (0, (0, (0,0)))
