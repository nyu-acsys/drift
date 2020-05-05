

let rec sum n =
  if n <= 0
  then 0
  else n + sum (n - 1)

let main_p (mn:int) =
    assert (6 * mn - 15 <= sum mn)

let main (w:unit) =
	let _ = main_p 10 in
	let _ = main_p 47 in
(* let _ = 
    for i = 1 to 1000000 do
      main (Random.int 1000)
    done *)
	()

let _ = main ()