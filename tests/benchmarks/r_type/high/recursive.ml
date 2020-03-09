

let main n = 
	let rec f g x = if x >= 0 then g x else f (f g) (g x) in
	let succ sx = sx + 1 in
	assert (f succ n > n) 
in
main 1293
