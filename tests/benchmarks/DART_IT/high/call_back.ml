

let mk_adder (x:int) = 
	let adder (i:int) = i + x in
	adder

let main (mx:int(*-:{v:Int | true}*)) (mi:int(*-:{v:Int | true}*)) = 
	let addr = mk_adder mx in
	assert (addr mi = mi + mx)

let _ = main 10 3
let _ = main 30 2 
let _ = main 0 0
let _ = main (-1) (-5)

