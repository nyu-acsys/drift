
let main mn =
    let rec rev n m =
      if n = 0
      then m
      else rev (n - 1) (m + 1) in

    assert (rev mn 0 >= mn)
in
main 300