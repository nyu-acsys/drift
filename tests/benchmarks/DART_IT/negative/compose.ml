(*
 * Input data error
 *)
 

let compose x g h = g (h x)

let id (ix:int) = ix 

let add (ay:int) = ay + 1

let main (n:int(*-:{v:Int | v <= 0}*)) =
	assert(compose n add id > 1)

let _ = main (-100)
let _ = main 0