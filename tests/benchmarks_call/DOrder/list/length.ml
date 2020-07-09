let rec length (xs:int list) =
  match xs with
      [] -> 0 (*l=0 v=0*)
    | x::xs' -> 1 + length xs' (*n>=l>=1 v=1*)

let rec make_list n =
  if n = 0
  then [] (* 'a list *)
  else n :: make_list (n - 1)

let main (n:int) =
  if n > 0 then
	  let xs = make_list n in
	  assert (length xs = n)
  else ()

let _ = main 1
