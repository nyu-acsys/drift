

let rec loop (x:int) i =
  if i < 0 then
    x
  else if x < 1 then (* x <= 0*)
    loop (x - 1) (i - 1)
  else if x > 2 then (* x >= 3*)
    loop x (i - 1)
  else (* 1 2 *)
    loop (3 - x) (i - 1)
in 
let main (mx:int) n =
  loop mx n
in main 3 10