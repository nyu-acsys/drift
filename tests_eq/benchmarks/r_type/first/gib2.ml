(*
let rec gib a b n =
  if n=0 then a
  else if n=1 then b
  else gib a b (n-1) + gib a b (n-2)

let main n =
  assert (gib 0 1 n >= 0)
*)
let main n a b =
	let rec gib ga gb gn =
	  if gn = 0 then ga
	  else if gn = 1 then gb
	  else gib ga gb (gn - 1) + gib ga gb (gn - 2)
	in

	if a >= 0 && b >= 0 then
		gib a b n >= 0
	else false
in assert(main 10 3 5 = true)