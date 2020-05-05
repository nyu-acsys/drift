

let rec rev n m =
  if n = 0
  then m
  else rev (n - 1) (m + 1)

let main_p (mn:int) =
    if mn > 0 then assert (rev mn 0 >= mn)
	else ()

let main (w:unit) =
	let _ = main_p 300 in
(* let _ = 
    for i = 1 to 1000000 do
      main (Random.int 1000)
    done *)
	()

let _ = main ()