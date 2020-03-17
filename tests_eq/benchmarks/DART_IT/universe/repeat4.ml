

let succ sx = sx + 1

let rec repeat rf rn =
  if rn = 0
  then 0
  else rf (repeat rf (rn - 1))

let main (n(*-:{v:Int | v >= 0}*)) =
    if n >= 0 then 
    	assert (repeat succ n = n)
    else assert(true)

