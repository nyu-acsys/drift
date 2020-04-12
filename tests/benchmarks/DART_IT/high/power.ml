

let main (m(*-:{v:Int | true}*)) (n(*-:{v:Int | true}*)) = 

	let comp cf cg cx = cf (cg cx)
	in

	let id dx = dx
	in

	let rec power f i = 
	    if i = 0 then id
	    else comp f (power f (i - 1))
	in

	let succ sx = sx + 1
	in

    if m > 0 then
    	assert(power succ m n >= n)
	else assert(true)

(* let _ = main 4 2
let _ = main 0 0
let _ = main 423 0
let _ = main 203 403
let _ = main 22 (-100) *)