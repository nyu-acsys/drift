(*
Tuple Encoding of Product Program.

Source Program: 

let rec rev l =
  let rec aux l_aux l_acc =
    if l_aux = 0 then l_acc
    else begin ev 2; aux (l_aux-1) (l_acc+1) end
  in aux l 0

let rec dequeue (l1_deq, l2_deq) = 
  if (l1_deq > 0) then 
    begin ev 1; dequeue ((l1_deq-1), l2_deq) end
  else if l1_deq = 0 then
    (if l2_deq > 0 then dequeue ((rev l2_deq), 0)
          else 0) 
  else 0
  (* if (l1_deq = 0) then rev (l2_deq)
  else repeat_dequeue ((l1_deq -1), l2_deq) *)

let rec enqueue n_r (l1_eq, l2_eq) =
  if n_r = 0 then (l1_eq, l2_eq)
  else begin ev 0; enqueue (n_r-1) (l1_eq, l2_eq+1) end

let main (n:int(*-:{v:Int | v >= 0 }*)) (l1:int(*-:{v:Int | v >= 0 }*)) (l2:int(*-:{v:Int | v >= 0 }*)) =
  dequeue (enqueue n (l1, l2))


Property: 

QSet = [0;1];

(* [default state, error state] *)

delta = fun evx (q, (enq, deq, tick)) ->
    if (q=0 && evx = 0) then (q, (enq+1, deq, tick))
    else if (q=0 && evx = 1) then (q, (enq, deq+1, tick))
    else if (q=0 && evx = 2) then (q, (enq, deq, tick+1))
    else (1, (enq, deq, tick));

IniCfg = (0, (0,0,0));

assertFinal = fun (q, (enq, deq, tick)) -> q=0 && tick = enq+prefl2 && tick = deq-prefl1;
(* assertFinal = fun (q, (enq, deq, tick)) -> q=0 && tick = enq+prefl2; *)


*)

let main (prefn:int(*-:{cur_v:Int | cur_v = 0}*)) (prefl1:int(*-:{cur_v:Int | cur_v = 0}*)) (prefl2:int(*-:{cur_v:Int | cur_v = 0}*)) =

let ev_step0 evx cfg0 =
  (match cfg0 with 
   (q,acc0) -> (match acc0 with 
                (enq,deq,tick) -> (if ((q = 0) && (evx = 0)) then (q,((enq + 1),deq,tick))
                                  else
                                    (if ((q = 0) && (evx = 1)) then (q,(enq,(deq + 1),tick))
                                    else
                                      (if ((q = 0) && (evx = 2)) then (q,(enq,deq,(tick + 1)))
                                      else (1,(enq,deq,tick)))))))
in

let asst_final0 cfg1 =
  (match cfg1 with 
   (q,acc1) -> (match acc1 with 
                (enq,deq,tick) -> assert (((q = 0) && (tick = (enq + prefl2))) && (tick = (deq - prefl1)))))
in

let rev l cfg2 =
  let rec aux
    l_aux l_acc cfg3 =
       (if (l_aux = 0) then (l_acc,cfg3)
       else (((aux (l_aux - 1)) (l_acc + 1)) ((ev_step0 2) cfg3))) in 
    (((aux l) 0) cfg2)
in

let rec dequeue x3 cfg5 =
  (match x3 with 
   (l1_deq,l2_deq) -> (if (l1_deq > 0) then ((dequeue ((l1_deq - 1),l2_deq)) ((ev_step0 1) cfg5))
                      else
                        (if
                           (l1_deq = 0)
                           then
                           (if
                              (l2_deq > 0)
                              then
                              (match (match ((rev l2_deq) cfg5) with 
                                      (x5,cfg6) -> ((x5,0),cfg6)) with 
                                     (x6,cfg7) -> ((dequeue x6) cfg7))
                              else (0,cfg5))
                           else (0,cfg5))))
in

let rec enqueue n_r x2 cfg9 =
  (match x2 with 
   (l1_eq,l2_eq) -> (if (n_r = 0) then ((l1_eq,l2_eq),cfg9)
                    else (((enqueue (n_r - 1)) (l1_eq,(l2_eq + 1))) ((ev_step0 0) cfg9)))) 
in


  (match (match (((enqueue prefn) (prefl1,prefl2)) (0,(0,0,0))) with 
          (x9,cfg12) -> ((dequeue x9) cfg12)) with 
         (e0,cfg11) -> ((asst_final0 cfg11) ; e0))

[@@@assert "typeof(main) <: int -> int -> int -> int"]
