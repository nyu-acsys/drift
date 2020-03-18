

let succ sx = sx + 1

let rec repeat rf rn =
  if rn = 0
  then 0
  else rf (repeat rf (rn - 1))

let main n =
    assert (repeat succ n = n)

let _ = main 1023

