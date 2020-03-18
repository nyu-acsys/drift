

let rec sum n =
  if n <= 0
  then 0
  else n + sum (n - 1)

let rec sigma f n =
  if n <= 0
  then 0
  else f n + sigma f (n - 1)

let main mn =
    assert (sigma sum mn >= mn)

let _ = main 846