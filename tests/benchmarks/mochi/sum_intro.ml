(*
USED: PEPM2013 as sum_intro
*)

let add x y = x + y in
let rec sum n =
  if n <= 0 then
    0
  else
    add n (sum (n - 1))
in
let main n = assert (n <= sum n) in
main 1293