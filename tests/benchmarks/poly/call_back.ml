

let mk_adder x = 
	let adder i = i + x in
	adder
in

let main mx mi = 
	let addr = mk_adder mx in
	assert (addr mi = mi + mx)
in

let out = if main 10 3 
	then main 30 2 
	else false 
in
out

