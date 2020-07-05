(* Eratosthene's sieve *)

(* list_interval min max = [min; min+1; ...; max-1; max] *)
let rec list_interval min max =
  if min > max then [] 
	else min :: list_interval (min + 1) max

(* let main (m:int(*-:{v:Int | true}*)) (n:int(*-:{v:Int | true}*)) =
  if m > 0 && n > m then 
    let xs = list_interval m n in
    let ys = sieve n xs in
    assert (List.length ys <= List.length xs)
  else () *)

let main (m:int(*-:{v:Int | v > 0}*)) (n:int(*-:{v:Int | v > m}*)) =
  let xs = list_interval m n in xs
(*withtype {max:int | max >= 2} <> => int(max) -> int list*)