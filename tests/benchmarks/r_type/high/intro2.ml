(*
USED: PLDI2011 as intro2
*)

let main (n(*-:{v:Int | v >= 0}*)) = 
	let f x g = g (x + 1) in
	let h y = y > 0 in
	assert(f n h)
(* in
assert(main 0 = true) *)