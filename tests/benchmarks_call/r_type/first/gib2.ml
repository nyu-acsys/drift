(*
let rec gib a b n =
  if n=0 then a
  else if n=1 then b
  else gib a b (n-1) + gib a b (n-2)

let main n =
  assert (gib 0 1 n >= 0)
*)

let rec gib ga gb gn =
  if gn = 0 then ga
  else if gn = 1 then gb
  else gib ga gb (gn - 1) + gib ga gb (gn - 2)

let main_p (n:int) (a:int) (b:int) =
	if a >= 0 && b >= 0 && n >= 0 then
		assert(gib a b n >= 0)
	else ()

let main (w:unit) =
	let _ = main_p 10 3 5 in
(* let _ = 
    for i = 1 to 1000000 do
      main (Random.int 1000) (Random.int 1000) (Random.int 1000)
    done *)
	()

let _ = main ()
