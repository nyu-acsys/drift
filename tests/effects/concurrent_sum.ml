
let choose () = 
  if nondet then
    ev 1; 0
  else if nondet then
    ev 2; 0
  else 
    ev 3; 0
m
let rec sum n f =
  if n = 0 then 0
  else (f ()) + (sum (n-1) f)

let main (n: int(*-:{v:Int | v > 0}*) = 
  sum n choose
