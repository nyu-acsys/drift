let rec iter (f:int -> unit) xs =
  match xs with
      [] -> ()
    | x::xs' -> f x; iter f xs'

let rec make_list n =
  if n < 0
  then []
  else n :: make_list (n - 1)

let check x = assert (x >= 0)

let main (n:int) =
	if n > 0 then
		let xs = make_list n in
	    iter check xs
	else ()

let _ = main 3