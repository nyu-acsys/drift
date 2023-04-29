let next x = 
  ev x;
  x

let rec loop s = 
  if (s > 0) then loop (next s)
  else ()

let main (u: unit) = 
  ev 0;
  loop 4

  
