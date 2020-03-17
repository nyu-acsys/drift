
let main (n(*-:{v:Int | v >= 0}*)) =
    let succ sx = sx + 1 in
    let rec repeat rf rn =
      if rn = 0
      then 0
      else rf (repeat rf (rn - 1))
    in

    assert (repeat succ n = n)

