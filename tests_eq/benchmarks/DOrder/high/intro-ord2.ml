
let rec app (a:int) (f:int->unit) (x:int) (k:int) = 
	if k > 0 then 
		app a f (x + 1) (k - 1)
	else f x

let check (cx:int) (cy:int) = assert (cx <= cy)

let main (i:int) (j:int) = app i (check i) i j

let _ = main 2 5
let _ = main 23 (-5)
let _ = main 0 2
let _ = main (-6) (-3)
let _ = main (-242) 4