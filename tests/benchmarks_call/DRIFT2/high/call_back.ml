
let mk_adder (x:int) = 
	let adder (i:int) = i + x in
	adder

let main_p (mx:int) (mi:int) = 
	let addr = mk_adder mx in
	assert (addr mi = mi + mx)

let main (w:unit) =
	let _ = main_p 10 3 in
    let _ = main_p 30 2 in
(* let _ = 
    for i = 1 to 1000000 do
      main (Random.int 1000) (Random.int 1000)
    done *)
	()

let _ = main ()