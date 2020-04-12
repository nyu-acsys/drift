
let main (mx(*-:{v:Int | true}*)) (mi(*-:{v:Int | true}*)) = 

	let mk_adder x = 
		let adder i = i + x in
		adder
	in

	let addr = mk_adder mx in
	assert (addr mi = mi + mx)

(* let _ = main 10 3
let _ = main 30 2 
let _ = main 0 0
let _ = main (-1) (-5)
 *)
