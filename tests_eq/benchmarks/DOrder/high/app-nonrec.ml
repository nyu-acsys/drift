

let apply ab af ax = af ax

let check cx cy = assert (cx = cy)

let main n =
apply n (check n) n

let _ = main 75