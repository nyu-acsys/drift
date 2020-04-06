

let rec sum n =
  if n <= 0
  then 0
  else n + sum (n - 1)

let rec sigma f n =
  if n <= 0
  then 0
  else f n + sigma f (n - 1)

let main (mn:int) =
    assert (sigma sum mn >= mn)

let _ = main 846
let _ = main 0
let _ = main 15
let _ = main 30
let _ = main (-43)
let _ = main 0
let _ = main (-3434)