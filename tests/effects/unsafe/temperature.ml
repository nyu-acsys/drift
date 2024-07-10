let rec f x pos neg = 
  if x mod 2 = 0 then 
    ev pos
  else
    ev neg;
  if (x <= 0) then 0 else f (x-2) (pos) (neg-1)


let main (v:int(*-:{v:Int | true}*)) (p:int(*-:{v:Int | true}*)) (n:int(*-:{v:Int | true}*)) =
  if p > 0 && n < 0 then
    f v p n
  else 0 
  
