(*let rec rev l = 
  let rec aux l_aux l_acc = 
    if l_aux = 0 then l_acc
    else begin ev 2; aux (l_aux-1) (l_acc+1) end
  in aux l 0


let is_empty (l1_e, l2_e) = l1_e = 0 && l2_e = 0

let dequeue (l1_dq, l2_dq) = 
  if l1_dq = 0 then ((rev l2_dq)-1, 0)
  else (l1_dq-1, l2_dq)

let rec repeat_dequeue (l1_dqr, l2_dqr) =
  if is_empty (l1_dqr, l2_dqr) then 0
  else begin ev 1; repeat_dequeue (dequeue (l1_dqr, l2_dqr)) end

let rec enqueue n_r (l1_eq, l2_eq) =
  if n_r = 0 then (l1_eq, l2_eq)
  else begin ev 0; enqueue (n_r-1) (l1_eq, l2_eq+1) end

let main (n:int(*-:{v:Int | true}*)) (l1:int(*-:{v:Int | true}*)) (l2:int(*-:{v:Int | true}*)) =
  if (l1>=0 && l2>=0 && n>=0) then
    repeat_dequeue (enqueue n (l1, l2))
  else
    0
*)
(* --- end of the current faithful encoding of the lics 18 paper *)


(* let rec rev l =  *)
(*   let rec aux l_aux l_acc =  *)
(*     if l_aux = 0 then l_acc *)
(*     else begin ev 2; aux (l_aux-1) (l_acc+1) end *)
(*   in aux l 0 *)


(* inlined
let is_empty (l1_e, l2_e) = l1_e = 0 && l2_e = 0
*)

(* let dequeue (l1_dq, l2_dq) =  *)
(*   ev 1; *)
(*   if l1_dq = 0 then ((rev l2_dq)-1, 0) *)
(*   else (l1_dq-1, l2_dq) *)

(* let rec repeat_dequeue (l1_dqr, l2_dqr) = *)
(*   if (l1_dqr = 0 && l2_dqr = 0) then 0 *)
(*   else repeat_dequeue (dequeue (l1_dqr, l2_dqr)) *)

(* let check_result (l1_chk, l2_chk) = 
  if (l1_chk = 0 && l2_chk = 0) then
    0 
  else 
    1
*) 

let rec rev l =
  let rec aux l_aux l_acc =
    if l_aux = 0 then l_acc
    else begin ev 2; aux (l_aux-1) (l_acc+1) end
  in aux l 0

let rec dequeue (l1_deq, l2_deq) = 
  if (l1_deq > 0) then 
    begin ev 1; dequeue ((l1_deq-1), l2_deq) end
  else if l1_deq = 0 then
    if l2_deq > 0 then dequeue ((rev l2_deq), 0)
    else 0
  (* if (l1_deq = 0) then rev (l2_deq)
  else repeat_dequeue ((l1_deq -1), l2_deq) *)

let rec enqueue n_r (l1_eq, l2_eq) =
  if n_r = 0 then (l1_eq, l2_eq)
  else begin ev 0; enqueue (n_r-1) (l1_eq, l2_eq+1) end

let main (n:int(*-:{v:Int | v >= 0 }*)) (l1:int(*-:{v:Int | v >= 0 }*)) (l2:int(*-:{v:Int | v >= 0 }*)) =
  dequeue (enqueue n (l1, l2))


(* let is_empty l1_e l2_e = l1_e = 0 && l2_e = 0

let rec dequeue l1_dq l2_dq f_dq = 
  if l1_dq = 0 then dequeue (rev l2_dq) 0 f_dq
  else begin ev 1; f_dq (l1_dq-1) l2_dq end

let rec enqueue n_r l1_eq l2_eq f_eq =
  if n_r = 0 then f_eq l1_eq l2_eq
  else begin ev 0; enqueue (n_r-1) l1_eq (l2_eq+1) f_eq end

let rec repeat_dequeue l1_dqr l2_dqr =
  if is_empty l1_dqr l2_dqr then 0
  else dequeue l1_dqr l2_dqr (fun a1 -> fun b1 -> repeat_dequeue a1 b1)

let main (n:int(*-:{v:Int | true}*)) (l1:int(*-:{v:Int | true}*)) (l2:int(*-:{v:Int | true}*)) =
  if (l1>=0 && l2>=0) then
    if n>=0 then 
      enqueue n l1 l2 (fun a2 -> fun b2 -> repeat_dequeue a2 b2)
    else repeat_dequeue l1 l2
  else
    0 *)
