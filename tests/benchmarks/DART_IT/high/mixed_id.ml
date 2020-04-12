
let main (k(*-:{v:Int | true}*)) = 
	let id_int x_idi = x_idi in

	let id_f x_idf = x_idf in

	let mid_y yk = id_int yk in

	let z x = id_f x in

	assert(z mid_y k = k)


(* let _ = main 10
let _ = main 20
let _ = main 0
let _ = main (-100)
let _ = main 1000 *)