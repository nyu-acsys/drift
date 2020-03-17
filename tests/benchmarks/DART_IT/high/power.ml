

let main m n = 

	let comp cf cg cx = cf (cg cx)
	in

	let id dx = dx
	in

	let rec power f i = 
	    if i = 0 then id
	    else comp f (power f (i - 1))
	in

	let succ sx = sx + 1
	in

    if m > 0 then
    	assert(power succ m n >= n)
	else assert(true)
in main 4 2