
let main (n(*-:{v:Int | true}*)) (m(*-:{v:Int | true}*)) = 

	let succ sb sf sx = sf (sx + 1) in

	let rec app b f x k = 
		if k > 0 then app (b - 1) (succ (b - 1) f) (x - 1) (k - 1) else f x
	in

	let check cx cy = assert (cx <= cy) in

	app n (check n) n m

(* let _ = main 5 4
let _ = main 30 (-2)
let _ = main 0 3
let _ = main (-4) (-6)
let _ = main (-102) 1 *)