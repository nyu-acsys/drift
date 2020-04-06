

let succ (sb:int) (sf:int->unit) sx = sf (sx + 1)

let rec app (b:int) (f:int->unit) x (k:int) = 
	if k > 0 then app (b - 1) (succ (b - 1) f) (x - 1) (k - 1) else f x
	
let check (cx:int) (cy:int) = assert (cx <= cy)

let main (n:int) (m:int) = app n (check n) n m

let _ = main 5 4
let _ = main 30 (-2)
let _ = main 0 3
let _ = main (-4) (-6)
let _ = main (-102) 1