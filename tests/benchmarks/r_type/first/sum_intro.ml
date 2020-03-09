(*
USED: PEPM2013 as sum_intro
*)
let main mn =
	let add x y = x + y in
	let rec sum n =
	  if n <= 0 then
	    0
	  else
	    add n (sum (n - 1))
	in
	assert (mn <= sum mn) in
main 1293