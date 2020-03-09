
let main n =
    let rec loop x i =
      if i < 0 then
        x
      else if x < 1 then
        loop (x - 1) (i - 1)
      else if x > 2 then
        loop x (i - 1)
      else
        loop (3 - x) (i - 1)
    in 

    assert (loop 1 n >= 0)
in main 4
