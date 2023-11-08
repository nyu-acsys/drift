

let f x pos neg = 
  if x mod 2 = 0 then 
    ev pos
  else
    ev neg;
  if (x <= 0) then 0 else f (x-1) pos neg


let main (v:int) (p:int) (n:int) =
  if p > 0 && n < 0 then
    f x p n
  else 0 
  