(*
USED: PLDI2011 as intro2
*)


let f x (g:int -> bool) = g (x + 1)

let h (y:int) = y > 0

let main (n:int) = 
	if n >= 0 then assert(f n h)
	else assert(true)
let _ = main 0
let _ = main 15
let _ = main 30
let _ = main (-43)
let _ = main 0
let _ = main (-3434)