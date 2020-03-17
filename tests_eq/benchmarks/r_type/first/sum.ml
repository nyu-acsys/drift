(*
USED: PLDI2011 as sum
USED: PEPM2013 as sum
*)
let main mn =
    let rec sum n =
      if n <= 0
      then 0
      else n + sum (n - 1)
    in

    assert (mn <= sum mn)
in main 2047