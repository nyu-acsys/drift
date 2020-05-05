
let id_int (x_idi:int) = x_idi

let id_f (x_idf:int -> int) = x_idf

let mid_y (yk:int) = id_int yk

let z x = id_f x

let main_p (k:int) = 
	assert(z mid_y k = k)

let main (w:unit) =
		let _ = main_p 20 in
    let _ = main_p 30 in
    let _ = main_p (-20) in
(* let _ = 
    for i = 1 to 1000000 do
      main (Random.int 1000)
    done *)
	()

let _ = main ()