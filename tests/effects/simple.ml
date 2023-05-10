let rec incr (i: int) =
  ev i;
  if (i > 0) then incr (i - 1)
  else ()
       
let main (u: unit) =
  incr 3
