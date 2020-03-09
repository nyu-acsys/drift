
let main mn = 
    let rec repeat f n s =
      if n = 0 then
        s
      else
        f (repeat f (n - 1) s)
    in
    let succ x = x + 1 in
    assert(repeat succ mn 0 >= mn) 
in
main 10