let next x = 
  ev x;
  x - 1

let rec loop s n = 
  if (s > 0) then loop (n s) n
  else ()

let main (u: unit) = 
  ev 0;
  loop 4 next

  
