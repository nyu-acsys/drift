(*
USED: PEPM2013 as sum_intro
*)

let add x y = x + y

let rec sum n =
  if n <= 0 then
    0
  else
    add n (sum (n - 1))

let main (mn:int) =
	assert (mn <= sum mn)

let _ = main 1293
let _ = main 15
let _ = main 30
let _ = main (-43)
let _ = main 0
let _ = main (-3434)