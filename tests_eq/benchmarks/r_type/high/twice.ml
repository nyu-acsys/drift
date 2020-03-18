

let twice tf tx = tf (tf tx)
let f fx = 2 * fx

let main n =
	if n > 0
	then assert (twice f n > n)

let _ = main 123