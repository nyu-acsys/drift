let rec f x = 
  ev x;
  if x <= 0 then 0
  else f (x-1)

let main (u: int(*-:{v:Int | true}*)) = 
 if u > 0 then f u else 0
