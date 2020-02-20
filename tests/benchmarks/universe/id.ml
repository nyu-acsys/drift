
let f x (z:bool) = 
	if z then x else 2 in

let id (k:int) = k in 

let main n = f (id n) true in
let res = main 3 in
assert(res >= 2 && res <= 3)