

let rec zip x y =
  if x = 0
  then
    if y = 0
    then 0
    else ( y)
  else
    if y = 0
    then ( x)
    else 1 + zip (x - 1) (y - 1)

let main (n:int) =
  assert (zip n n = n)

let _ = main 100
let _ = main 10
let _ = main 300
let _ = main 0
let _ = main (-34)