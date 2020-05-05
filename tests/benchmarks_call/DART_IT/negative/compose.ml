(*
 * Input data error
 *)
 
let compose x g h = g (h x)

let id (ix:int) = ix 

let add (ay:int) = ay + 1

let main_p (n:int) =
	if n <= 0 then assert(compose n add id > 1)
	else ()

let main (w:unit) =
	let _ = main_p (-100) in
    let _ = main_p 0 in
(* let _ = 
    for i = 1 to 1000000 do
      main (Random.int 1000)
    done *)
	()

let _ = main ()