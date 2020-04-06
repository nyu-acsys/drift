

let rec loop x i =
  if i < 0 then
    x
  else if x < 1 then
    loop (x - 1) (i - 1)
  else if x > 2 then
    loop x (i - 1)
  else
    loop (3 - x) (i - 1)

let main (n:int) =
    assert (loop (-3) n <= (-3))

let _ = main 10 
let _ = main 15
let _ = main 30
let _ = main (-43)
let _ = main 0
let _ = main (-3434)