(*
	False positive
*)

let main n i =
	let rec init idx tn ta =
	  if idx >= tn then ta
	  else (set ta idx 1; init (idx + 1) tn ta)
	in

	let x = init 0 3 (make 3 0) in
	if 0<=i && i < n then
    	get x i >= 1 (* check that the array has been initialized *)
    else false
in assert(main 3 2 = true)