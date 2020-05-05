
let id_int (x_idi:int) = x_idi

let id_f (x_idf:int -> int) = x_idf

let mid_y (yk:int) = id_int yk

let z x = id_f x

let main (k:int(*-:{v:Int | true}*)) = 
	assert(z mid_y k = k)