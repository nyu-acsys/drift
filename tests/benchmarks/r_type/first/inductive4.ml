

let rec f g x =
  if x < (-3) then (* x<=-4 *)
    f g (-4)
  else if x <= 1 then (* -3<=x<=1 *)
    g x
  else (* x>1 *)
    f (f g) (x - 2)

let incr y = y + 1

let main (n:int(*-:{v:Int | true}*)) =   
    assert(f incr 3 >= -3)
