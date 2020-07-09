let rec fold_left f (acc:int) (xs:int list) =
  match xs with
      [] -> acc
    | x::xs' -> fold_left f (f acc x) xs'

let rec make_list n =
  if n <= 0 then []
  else n :: make_list (n - 1)

let div x y = (assert (y <> 0); x / y)

let main (n:int) (m:int) =
  if n > 0 && m > 0 then
  	let xs = make_list n in
  	assert(fold_left div m xs >= 0)
  else ()

let _ = main 1 1