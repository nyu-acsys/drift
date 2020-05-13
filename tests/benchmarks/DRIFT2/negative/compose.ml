(*
 * Input data error
 *)
 

let compose x g h = g (h x)

let id (ix:int) = ix 

let add (ay:int) = ay + 1

let main (n:int(*-:{v:Int | true}*)) =
	if n <= 0 then assert(compose n add id > 1)
	else ()