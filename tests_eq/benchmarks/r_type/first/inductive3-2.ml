

let rec f x =
  if x < -1 then
    f (-2)
  else if x <= 1 then
    2 * x - 1
  else
    x

let main (n:int) =
    if n >= -1 then assert(f n >= -3)
    else assert(true)
let _ = main (-1)
let _ = main 15
let _ = main 30
let _ = main (-43)
let _ = main 0
let _ = main (-3434)