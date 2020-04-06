

let succ sx = sx + 1

let rec repeat rf rn =
  if rn = 0
  then 0
  else rf (repeat rf (rn - 1))

let main (n:int) =
    assert (repeat succ n = n)

let _ = main 1023
let _ = main 0
let _ = main 15
let _ = main 30
let _ = main (-43)
let _ = main 0
let _ = main (-3434)