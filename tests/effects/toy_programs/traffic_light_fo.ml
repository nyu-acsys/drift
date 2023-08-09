let next x i = 
  ev x;
  x - i

let rec loop s t = 
  if (s > 0) then loop (next s t) t
  else ()

let main (u: unit) = 
  ev 0;
  loop 4 1

  
