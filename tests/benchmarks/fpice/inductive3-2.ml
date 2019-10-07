let rec f x =
  if x < -1 then
    f (-2)
  else if x <= 1 then
    2 * x - 1
  else
    x
in
let main n =
	if n >= -1 then assert(f n >= -3) else false
in main -1