
let main (n(*-:{v:Int | v >= 2}*)) =
    let rec f x =
      if x < -1 then
        f (-2)
      else if x <= 1 then
        2 * x - 1
      else
        x
    in

    assert(f n >= 0)
(* in assert(main 17 = true) *)