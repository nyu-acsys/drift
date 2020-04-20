

let rec init idx tn (ta: int array) =
  if idx >= tn then ()
  else (Array.set ta idx 1; init (idx + 1) tn ta)

let main (n(*-:{v:Int | true}*)) (i(*-:{v:Int | true}*)) =
	let res:bool = 
		if i >= 0 && i < n then
			let a = Array.make n 0 in
			init i n a;
    		let item = Array.get a i in
    		item >= 1 (* check that the array has been revised *)
    	else true
    in assert(res = true)

let _ = main 10 3