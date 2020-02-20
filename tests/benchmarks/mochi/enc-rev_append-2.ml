let rec append x y =
  if x = 0 then
    y
  else
    1 + append (x - 1) y
in

let rec rev rn =
  if rn = 0
  then 0
  else append (rev (rn - 1)) 1
in

let main n m =
    assert (append n m = n + m)
in
main 3 5