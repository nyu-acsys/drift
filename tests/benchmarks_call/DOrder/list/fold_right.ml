let rec fold_right (f:int->int->int) xs acc =
  match xs with
    [] -> acc
  | x::xs' -> f x (fold_right f xs' acc)

let rec make_list n =
  if n < 0
  then []
  else n :: make_list (n - 1)

let add x y = x + y

let main (n:int) (m:int) =
  if n > 0 && m > 0 then
  	let xs = make_list n in
    assert (fold_right add xs m >= m)
  else ()

let _ = main 1 1