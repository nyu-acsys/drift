
let main (n(*-:{v:Int | v > 0}*)) =
	let twice tf tx = tf (tf tx) in
	let f fx = 2 * fx in

	assert (twice f n > n)
(* in main 123 *)