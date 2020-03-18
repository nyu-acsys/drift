
let main (mx(*-:{v:Int | true}*)) (my(*-:{v:Int | true}*)) = (*(*-:{v:Int | v > mx}*)*)

	let twice tf tx ty = tf (tf tx ty) ty
	in

	let flip f x y = f y x
	in

	let square_diff sx sy = sx * sx - sy * sy
	in

	if mx >= 0 && my >= 0 then
     	if mx > my then
        	assert(flip (twice square_diff) my mx >= mx + my)
    	else if mx < my then assert(flip (twice square_diff) mx my >= mx + my)
    	else assert(true)
    else assert(true)

(*in main 16 32*)