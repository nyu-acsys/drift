(*
 * Input data error
 *)

let main (mn(*-:{v:Int | v < 0}*)) =
	let rec repeat f n s =
	  if n = 0 then
	    s
	  else
	    f (repeat f (n - 1) s)
	in

	let succ x = x + 1 in

	assert(repeat succ mn 0 >= mn)
	
(* in
main (-1) Overflow *)