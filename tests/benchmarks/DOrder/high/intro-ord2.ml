
let main (i(*-:{v:Int | true}*)) (j(*-:{v:Int | true}*)) = 
	let rec app a f x k = 
		if k > 0 then 
			app a f (x + 1) (k - 1)
		else f x
	in

	let check cx cy = assert (cx <= cy) in

	app i (check i) i j

(* let _ = main 2 5
let _ = main 23 (-5)
let _ = main 0 2
let _ = main (-6) (-3)
let _ = main (-242) 4 *)