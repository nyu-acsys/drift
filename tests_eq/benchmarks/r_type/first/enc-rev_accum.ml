

let rec rev n m =
  if n = 0
  then m
  else rev (n - 1) (m + 1)

let main (mn:int) =
    if mn > 0 then assert (rev mn 0 >= mn)
	else assert(true)

let _ = main 300
let _ = main 120
let _ = main 0
let _ = main (-34)
let _ = main (-856)