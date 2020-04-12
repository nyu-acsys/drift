

let main (mx(*-:{v:Int | true}*)) = 
	let twice tf tx ty = tf (tf tx ty) ty
	in

	let flip f x y = f y x
	in

	let square_diff sx sy = (sx + sy) * (sx - sy)
	in

    let res = flip (twice square_diff) mx mx
    in
    assert(res = mx * (0 - mx))

(* let _ = main 6
let _ = main 0
let _ = main 100
let _ = main (-203)
let _ = main 1033 *)