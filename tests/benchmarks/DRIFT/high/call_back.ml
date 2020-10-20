

let mk_adder (x:int) = 
	let adder (i:int) = i + x in
	adder

let main (mx:int(*-:{v:Int | true}*)) (mi:int(*-:{v:Int | true}*)) = 
	let addr = mk_adder mx in
	assert (addr mi = mi + mx)
