


	let rec loop x i =
	  if i < 0 then
	    x
	  else if x < 1 then (* x <= 0*)
	    loop (x - 1) (i - 1)
	  else if x > 2 then (* x >= 3*)
	    loop x (i - 1)
	  else (* 1 2 *)
	    loop (3 - x) (i - 1)

let main (m(*-:{v:Int | v=2}*)) (n(*-:{v:Int | true}*)) =
  	let ans = loop m n in
  	if m = 2 then
  		assert(ans >= 1 && ans <= 2)
  	else assert(true)

let _ = main 2 4