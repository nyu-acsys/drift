
let main (n(*-:{v:Int | v >= -1}*)) =
    let rec f x =
      if x < -1 then
        f (-2)
      else if x <= 1 then
        2 * x - 1
      else
        x
    in

    assert(f n >= -3)
(* in assert(main (-1) = true) *)