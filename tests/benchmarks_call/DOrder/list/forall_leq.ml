let rec for_all f (xs:int list) =
  match xs with
      [] -> 1
    | x::xs' ->
        if (f x > 0) && (for_all f xs' > 0) then 1
				else 0

let check x = 
	if (x >= 0) then 1
	else 0

let rec make_list n =
  if n < 0
  then []
  else n :: make_list (n - 1)

let main (n:int) = 
  if n > 0 then
    assert (for_all check (make_list n) > 0)
  else ()

let _ = main 3