
let rec app (a:int) (f:int->unit) (x:int) (k:int) = 
	if k > 0 then 
		app a f (x + 1) (k - 1)
	else f x

let check (cx:int) (cy:int) = assert (cx <= cy)

let main (i:int(*-:{v:Int | true}*)) (j:int(*-:{v:Int | true}*)) = app i (check i) i j
