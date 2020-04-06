

let apply (ab:int) (af: int->unit) ax = af ax

let check (cx:int) (cy:int) = assert (cx = cy)

let main (n:int) =
	apply n (check n) n

let _ = main 75
let _ = main 0
let _ = main 100
let _ = main (-53)
let _ = main (-573)