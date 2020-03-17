
let main (mn(*-:{v:Int | v >= 0}*)) =
    let rec sum n =
      if n <= 0
      then 0
      else n + sum (n - 1)
    in

    let rec sigma f n =
      if n <= 0
      then 0
      else f n + sigma f (n - 1)
    in

    assert (sigma sum mn >= mn)
