(*
 * Input data error
 *)
 

let compose x g h = g (h x)

let id (ix:int) = ix 

let add (ay:int) = ay + 1

let main (n:int) =
	if n <= 0 then assert (compose n add id > 1)
	else ()

let _ = main (-100)
let _ = main 0
let _ = main 34