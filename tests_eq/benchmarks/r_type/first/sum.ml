(*
USED: PLDI2011 as sum
USED: PEPM2013 as sum
*)

let rec sum n =
  if n <= 0
  then 0
  else n + sum (n - 1)

let main (mn:int) =
    assert (mn <= sum mn)

let _ = main 2047
let _ = main 15
let _ = main 30
let _ = main (-43)
let _ = main 0
let _ = main (-3434)