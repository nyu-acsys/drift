(*
USED: PLDI2011 as repeat
*)


let succ sx = sx + 1

let rec repeat rf rn rs =
  if rn = 0 then
    rs
  else
    rf (repeat rf (rn - 1) rs)

let main n =
	assert (repeat succ n 0 = n)

let _ = main 103
