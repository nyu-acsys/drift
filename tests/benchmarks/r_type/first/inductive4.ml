
let main (n(*-:{v:Int | true}*)) =
    let rec f g x =
      if x < (-3) then (* x<=-4 *)
        f g (-4)
      else if x <= 1 then (* -3<=x<=1 *)
        g x
      else (* x>1 *)
        f (f g) (x - 2)
    in
    let incr y = y + 1
    in

    assert(f incr n >= -3)
(* in main 12 *)
