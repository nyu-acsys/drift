let rec append x y =
  if x = 0 then
    y
  else
    1 + append (x - 1) y
in

let rec rev n =
  if n = 0
  then 0
  else append (rev (n - 1)) 1
in

let main n m =
    assert (rev n = n)
in
main 3 5