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

let ev_step0 evx cfg0 = (match cfg0 with 
                         (q,acc0) -> (match acc0 with 
                                      (enq,deq,tick) -> if ((q = 0) && (evx = 0)) then (q,((enq + 1),deq,tick))
                                                        else
                                                          if ((q = 0) && (evx = 1)) then (q,(enq,(deq + 1),tick))
                                                          else if ((q = 0) && (evx = 2)) then (q,(enq,deq,(tick + 1)))
                                                               else (1,(enq,deq,tick))))


let asst_final0 cfg1 = (match cfg1 with 
                        (q,acc1) -> (match acc1 with 
                                     (enq,deq,tick) -> assert (((q = 0) && (tick = (enq + prefl2))) && (tick = (deq - prefl1)))))


let rec rev l cfg2 = let aux0 = ((let rec aux l_aux cfg5 =
                                    ((fun l_acc cfg6 ->
                                        if (l_aux = 0) then (l_acc,cfg6)
                                        else (match (match ((aux (l_aux - 1)) ((ev_step0 2) cfg6)) with 
                                                     (x3,cfg7) -> ((x3 (l_acc + 1)) cfg7)) with  (x4,cfg8) -> ((() ; x4),cfg8))),cfg5)
                                        in aux),cfg2) in 
                                (match aux0 with 
                                 (aux,cfg3) -> (match ((aux l) cfg3) with 
                                                (x2,cfg4) -> ((x2 0) cfg4)))


let rec dequeue (l1_deq,l2_deq) cfg9 = if
                                         (l1_deq > 0)
                                         then
                                         (match ((dequeue ((l1_deq - 1),((ev_step0 1) cfg9))) (l2_deq,((ev_step0 1) cfg9))) with 
                                          (x5,cfg10) -> ((() ; x5),cfg10))
                                         else if (l1_deq = 0) then if (l2_deq > 0) then ((dequeue ((rev l2_deq) cfg9)) (0,cfg9))
                                                                   else (0,cfg9)
                                              else (0,cfg9)


let rec enqueue n_r cfg11 = ((fun (l1_eq,l2_eq) cfg12 ->
                                if (n_r = 0) then ((l1_eq,cfg12),(l2_eq,cfg12))
                                else (match (match ((enqueue (n_r - 1)) ((ev_step0 0) cfg12)) with 
                                             (x6,cfg13) -> ((x6 (l1_eq,cfg13)) ((l2_eq + 1),cfg13))) with  (x7,cfg14) -> ((() ; x7),cfg14))),cfg11) 


let main (n:int(*-:{cur_v:Int | cur_v = 0}*)) (l1:int(*-:{cur_v:Int | cur_v = 0}*)) (l2:int(*-:{cur_v:Int | cur_v = 0}*)) = (match (match (match ((enqueue n) (0,(0,0,0))) with 
                                                                                                                                           (x8,cfg15) -> ((x8 (l1,cfg15)) (l2,cfg15))) with 
                                                                                                                                          (x9,cfg16) -> ((dequeue x9) cfg16)) with 
                                                                                                                                    (e0,acfg0) -> ((asst_final0 acfg0) ; e0))