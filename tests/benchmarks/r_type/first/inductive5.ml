
let main (n(*-:{v:Int | v > -2}*)) =
	let rec f x =
	  if x < -2 then
	    f (-3)
	  else if x < 2 then
	    2 * x - 1
	  else if x <= 2 then
	    f (2 * x - 1)
	  else
	    x
	in

	assert(f n >= (-3))
(* in assert(main 3 = true) *)