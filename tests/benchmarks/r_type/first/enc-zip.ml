let rec zip x y =
  if x = 0
  then
    if y = 0
    then 0
    else (assert(false); y)
  else
    if y = 0
    then (assert(false); x)
    else 1 + zip (x - 1) (y - 1)
in

let main n =
  assert (zip n n = n)
in
main 100