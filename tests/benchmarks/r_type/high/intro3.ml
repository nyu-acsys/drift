(*
USED: PLDI2011 as intro3
*)

let main (n(*-:{v:Int | v >= 0}*)) = 
	let f x g = g (x + 1) in
	let h z y = y > z in
	assert(f n (h n))
(* in
assert(main 1263 = true) *)