let rec f x = 
  ev x;
  if x <= 0 then 0
  else f (x-1)

let main (u: unit(*-:{v:Unit | unit}*)) = 
 f 10
