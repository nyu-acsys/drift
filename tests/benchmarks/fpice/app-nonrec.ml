

let apply ab af ax = af ax
in

let check cx cy = assert (cx = cy)
in

let main n = apply n (check n) n
in
main 75