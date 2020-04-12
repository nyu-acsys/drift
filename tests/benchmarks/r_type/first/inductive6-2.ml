
let main (n(*-:{v:Int | v >= 3}*)) =
	let f g x =
	  if x > 0 then
	    g x
	  else
	    1
	in
	let decr x = x - 1 in

	assert(f decr n > 0)
(* in
main 79 *)