let rec f x =
  if x < -2 then
    f (-3)
  else if x < 2 then
    2 * x - 1
  else if x <= 2 then
    f (2 * x - 1)
  else
    x
in
let main n =
  if n > -2 then assert(f n >= -3) else false
in main 3