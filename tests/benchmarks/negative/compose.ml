
let compose x g h = g (h x) in

let id ix = ix in 

let add ay = ay + 1 in

let main n = 
	assert (compose n add id > 1)
in main (-100)