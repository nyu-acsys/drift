

let rec f x =
  if x < -1 then
    f (-2)
  else if x <= 1 then
    2 * x - 1
  else
    x

let main (n:int(*-:{v:Int | true}*)) =   
    if n >= 2 then assert(f n >= 0)
    else assert(true)

let _ = main 2