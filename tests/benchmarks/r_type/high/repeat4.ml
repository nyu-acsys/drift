

let succ sx = sx + 1

let rec repeat (rf: int -> int) rn =
  if rn = 0
  then 0
  else rf (repeat rf (rn - 1))

let main (n:int(*-:{v:Int | true}*)) =
    assert (repeat succ n = n)
