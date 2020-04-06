

let twice tf tx = tf (tf tx)
let f fx = 2 * fx

let main (n:int) =
	if n > 0
	then assert (twice f n > n)
	else assert(true)

let _ = main 123
let _ = main 0
let _ = main 15
let _ = main 30
let _ = main (-43)
let _ = main 0
let _ = main (-3434)