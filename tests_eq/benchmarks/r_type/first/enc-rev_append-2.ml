

let rec append x y =
  if x = 0 then
    y
  else
    1 + append (x - 1) y

let rec rev n =
  if n = 0
  then 0
  else append (rev (n - 1)) 1

let main (mn:int) (mm:int) =
    if mn >= 0 && mm >= 0 then
    	assert (append mn mm = mn + mm)
    else assert(true)

let _ = main 3 5
let _ = main 230 302
let _ = main 0 0
let _ = main (-34) 2
let _ = main 23 (-4)