

let rec rev n m =
  if n = 0
  then m
  else rev (n - 1) (m + 1)

let main (n:int(*-:{v:Int | true}*)) =
    assert (rev n 0 >= n)