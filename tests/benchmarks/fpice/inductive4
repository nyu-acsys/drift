let rec f g x =
  if x < -3 then
    f g (-4)
  else if x <= 1 then
    g x
  else
    f (f g) (x - 2)
in
let incr y = y + 1
in
let main n =
  assert(f incr 3 >= -3)
in main 12