(*
USED: PLDI2011 as intro3
*)


let f x g = g (x + 1)

let h z y = y > z

let main n = 
	if n >= 0 then assert(f n (h n))

let _ = main 1263