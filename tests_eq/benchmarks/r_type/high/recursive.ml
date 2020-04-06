


let rec f g x = if x >= 0 then g x else f (f g) (g x)
let succ sx = sx + 1

let main (n:int) = 
	assert (f succ n > n) 

let _ = main 1293
let _ = main 0
let _ = main 15
let _ = main 30
let _ = main (-43)
let _ = main 0
let _ = main (-3434)