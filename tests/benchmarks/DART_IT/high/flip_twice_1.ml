
let main (mx(*-:{v:Int | v >= 0}*)) (my(*-:{v:Int | v >= 0}*)) = (*(*-:{v:Int | v > mx}*)*)

	let twice tf tx ty = tf (tf tx ty) ty
	in

	let flip f x y = f y x
	in

	let square_diff sx sy = (sx + sy) * (sx - sy)
	in

	if mx > my then
        assert(flip (twice square_diff) my mx >= mx + my)
    else if mx < my then assert(flip (twice square_diff) mx my >= mx + my)
    else assert(true)

(* let _ = main 16 32
let _ = main 30 200
let _ = main 102 20
let _ = main 0 0
let _ = main 0 1
let _ = main 1 0
let _ = main 1203 2024
let _ = main (-3) 234
let _ = main 123 (-4) *)