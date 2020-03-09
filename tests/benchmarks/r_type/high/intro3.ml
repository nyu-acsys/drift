(*
USED: PLDI2011 as intro3
*)

let main n = 
	let f x g = g (x + 1) in
	let h z y = y > z in
	if n >= 0 then f n (h n) else false 
in
assert(main 1263 = true)