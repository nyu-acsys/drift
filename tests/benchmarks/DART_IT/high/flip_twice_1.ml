
let main mx my = 

	let twice tf tx ty = tf (tf tx ty) ty
	in

	let flip f x y = f y x
	in

	let square_diff sx sy = sx * sx - sy * sy
	in

    let res = if mx > my then
        flip (twice square_diff) mx my
        else if mx < my then flip (twice square_diff) my mx
        else mx * my
    in
    assert(res >= mx * my)
in main 16 32