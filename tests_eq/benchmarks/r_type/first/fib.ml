
let rec fib n =
  if n <= 1 then 1 else
    fib (n - 1) + fib (n - 2)

let main (mn:int) =
    assert (fib mn >= mn)

let _ = main 34
let _ = main 10
let _ = main 300
let _ = main 0
let _ = main (-34)
