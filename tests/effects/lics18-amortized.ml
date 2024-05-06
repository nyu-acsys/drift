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