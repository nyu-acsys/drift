
let main (i(*-:{v:Int | true}*)) (j(*-:{v:Int | true}*)) = 
	let succ sb sf sx = sf (sx + 1) in

	let rec app3 b f a g k = 
		if k > 0 then app3 b (succ b f) b g (k - 1) else g b f
	in

	let app ax ab af = af ax in

	let check cx cy = assert (cx <= cy) in

	app3 i (check i) i (app i) j

(* let _ = main 4 7
let _ = main 15 (-12)
let _ = main 2 0
let _ = main 0 (-24)
let _ = main (-4) (-5)
let _ = main (-12) 34 *)