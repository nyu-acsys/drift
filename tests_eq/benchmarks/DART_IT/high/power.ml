



let comp cf cg cx = cf (cg cx)

let id dx = dx

let rec power f i = 
    if i = 0 then id
    else comp f (power f (i - 1))

let succ sx = sx + 1

let main m n = 
	if m > 0 then
    	assert(power succ m n >= n)
	else assert(true)

let _ = main 4 2