
let main n =
	let twice tf tx = tf (tf tx) in
	let f fx = 2 * fx in

	if n > 0
	then assert (twice f n > n)
	else false
in main 123