
let main (mx(*-:{v:Int | true}*)) = 
	let f x y = assert (x <= 0 || y > 0) in
 	f mx mx
(* in
main 10 *)
