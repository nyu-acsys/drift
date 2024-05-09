let next x = 
  ev x;
  x - 1 

let rec loop s = 
  if (s > 0) then loop (next s)
  else ()

let main (u: unit(*-:{v:Unit | unit}*)) = 
  ev 0;
  loop 4

