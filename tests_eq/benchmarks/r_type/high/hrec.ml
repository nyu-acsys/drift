(*
USED: PLDI2011 as hrec
*)


let rec f fg fx = if fx >= 0 then fg fx else f (f fg) (fg fx)
let succ sx = sx + 1

let main (n:int) = 
	assert (f succ n >= 0)

let _ = main (-44)
let _ = main 15
let _ = main 30
let _ = main (-43)
let _ = main 0
let _ = main (-3434)