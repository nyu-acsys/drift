let rec repeat f n s =
  if n = 0 then
    s
  else
    f (repeat f (n - 1) s)
in
let succ x = x + 1 in
let main n = assert(repeat succ n 0 >= n) in
main 10