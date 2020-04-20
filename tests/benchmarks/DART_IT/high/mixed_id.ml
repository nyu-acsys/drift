
let id_int (x_idi:int) = x_idi

let id_f (x_idf:int -> int) = x_idf

let mid_y (yk:int) = id_int yk

let z x = id_f x

let main (k:int(*-:{v:Int | true}*)) = 
	assert(z mid_y k = k)


let _ = main 10
let _ = main 20
let _ = main 0
let _ = main (-100)
let _ = main 1000