let rec reent d = 
  ev 1; (* acquire *) 
  if (d > 0) then 
    begin
      if nondet then begin reent (d-1); ev (-1) (* release *) end
      else ()
    end
  else 
    ()

let main (n: int(*-:{v:Int | v > 0}*)) = 
  reent n; ev (-1) (* release *)

