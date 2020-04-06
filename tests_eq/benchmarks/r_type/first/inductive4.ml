

let rec f g x =
  if x < (-3) then (* x<=-4 *)
    f g (-4)
  else if x <= 1 then (* -3<=x<=1 *)
    g x
  else (* x>1 *)
    f (f g) (x - 2)

let incr y = y + 1

let main (n:int) =
    assert(f incr n >= -3)

let _ = main 12
let _ = main 15
let _ = main 30
let _ = main (-43)
let _ = main 0
let _ = main (-3434)
