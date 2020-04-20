
let rec repeat (f:int->int) n s =
  if n = 0 then
    s
  else
    f (repeat f (n - 1) s)

let succ x = x + 1

let main (mn:int(*-:{v:Int | true}*)) =
    assert(repeat succ mn 0 >= mn) 

let _ = main 2