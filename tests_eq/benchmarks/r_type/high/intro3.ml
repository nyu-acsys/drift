(*
USED: PLDI2011 as intro3
*)


let f (x:int) (g:int -> bool) = g (x + 1)

let h (z:int) (y:int) = y > z

let main (n:int) = 
	if n >= 0 then assert(f n (h n))
	else assert(true)
let _ = main 1263
let _ = main 15
let _ = main 30
let _ = main (-43)
let _ = main 0
let _ = main (-3434)