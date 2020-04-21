

let comp (cf:int -> int) (cg:int -> int) cx = cf (cg cx)

let id (dx:int) = dx

let rec power (f: int -> int) i = 
    if i = 0 then id
    else comp f (power f (i - 1))

let succ (sx:int) = sx + 1

let main (m(*-:{v:Int | true}*)) (n(*-:{v:Int | true}*)) = 
	if m > 0 then
    	assert(power succ m n >= n)
	else assert(true)

let _ = main 4 2
let _ = main 0 0
let _ = main 423 0
let _ = main 203 403
let _ = main 22 (-100)