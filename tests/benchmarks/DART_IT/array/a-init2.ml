
let main (n(*-:{v:Int | v >= 0}*)) (i(*-:{v:Int | v >= 0}*)) =
	let rec init idx tn ta =
	  if idx >= tn then ta
	  else (set ta idx 1; init (idx + 1) tn ta)
	in

	let x = init i n (make n 0) in
	let res = 
		if i < n then
    		get x i >= 1 (* check that the array has been initialized *)
    	else true
    in assert(res = true)