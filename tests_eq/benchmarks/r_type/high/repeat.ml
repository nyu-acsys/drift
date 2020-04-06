(*
USED: PLDI2011 as repeat
*)


let succ sx = sx + 1

let rec repeat rf rn rs =
  if rn = 0 then
    rs
  else
    rf (repeat rf (rn - 1) rs)

let main (n:int) =
	assert (repeat succ n 0 = n)

let _ = main 103
let _ = main 0
let _ = main 15
let _ = main 30
let _ = main (-43)
let _ = main 0
let _ = main (-3434)