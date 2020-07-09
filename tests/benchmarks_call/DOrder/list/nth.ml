let rec nth n (xs:int list) =
  match xs with
    | [] -> -1
    | x::xs' -> if n = 0 then x else nth (n - 1) xs'

let rec make_list n =
  if n < 0
  then []
  else n :: make_list (n - 1)

let main (n:int) = 
  if n > 0 then
		let xs = make_list n in
		assert(nth (n - 1) xs >= 0)
  else ()

let _ = main 1