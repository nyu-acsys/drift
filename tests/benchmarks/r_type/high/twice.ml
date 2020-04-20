

let twice (tf: int -> int) tx = tf (tf tx)
let f fx = 2 * fx

let main (n:int(*-:{v:Int | true}*)) =
	if n > 0
	then assert (twice f n > n)
	else assert(true)

let _ = main 123