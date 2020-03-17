
let main n =
    let rec f x =
      if x < -1 then
        f (-2)
      else if x <= 1 then
        2 * x - 1
      else
        x
    in

    if n >= -1 then f n >= -3 else false
in assert(main (-1) = true)