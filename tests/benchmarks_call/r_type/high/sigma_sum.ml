

let rec sum n =
  if n <= 0
  then 0
  else n + sum (n - 1)

let rec sigma (f: int -> int) n =
  if n <= 0
  then 0
  else f n + sigma f (n - 1)

let main_p (mn:int) =
    assert (sigma sum mn >= mn)

let main (w:unit) =
	let _ = main_p 54 in
(* let _ = 
    for i = 1 to 1000000 do
      main (Random.int 1000)
    done *)
	()

let _ = main ()