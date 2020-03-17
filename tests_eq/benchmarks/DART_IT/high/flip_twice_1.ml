

let twice tf tx ty = tf (tf tx ty) ty

let flip f x y = f y x

let square_diff sx sy = sx * sx - sy * sy

let main mx my = 
    if mx >= 0 && my >= 0 then
     	if mx > my then
        	assert(flip (twice square_diff) my mx >= mx + my)
    	else if mx < my then assert(flip (twice square_diff) mx my >= mx + my)
    	else assert(true)
    else assert(true)

let _ = main 16 32