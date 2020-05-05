

let twice (tf: int -> int) tx = tf (tf tx)
let f fx = 2 * fx

let main_p (n:int) =
	if n > 0
	then assert (twice f n > n)
	else ()

let main (w:unit) =
	let _ = main_p 123 in
(* let _ = 
    for i = 1 to 1000000 do
      main (Random.int 1000)
    done *)
	()

let _ = main ()