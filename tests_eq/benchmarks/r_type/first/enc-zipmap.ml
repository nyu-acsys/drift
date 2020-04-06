(*
USED: PLDI2011 as l-zipmap
USED: PEPM2013 as l-zipmap
*)

let rec zip x y =
  if x = 0 then
    if y = 0 then
      x
    else
      (y)
  else
    if y = 0 then
      (x)
    else
      1 + zip (x - 1) (y - 1)

let rec map x =
  if x <= 0 then x else 1 + map (x - 1)

let main (n:int) =
  assert (map (zip n n) = n)

let _ = main 31
let _ = main 10
let _ = main 300
let _ = main 0
let _ = main (-34)