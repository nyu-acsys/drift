

let succ (sb:int) (sf:int->unit) sx = sf (sx + 1)

let rec app (b:int) (f:int->unit) x (k:int) = 
	if k > 0 then app (b - 1) (succ (b - 1) f) (x - 1) (k - 1) else f x
	
let check (cx:int) (cy:int) = assert (cx <= cy)

let main (n:int(*-:{v:Int | true}*)) (m:int(*-:{v:Int | true}*)) = app n (check n) n m
