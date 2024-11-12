let rec f x b pos neg = 
  if b = 0 then 
    ev pos
  else
    ev neg;
  if (x <= 0) then 0 else f (x-1) b pos neg


let main (v:int(*-:{v:Int | true}*)) (p:int(*-:{v:Int | true}*)) (n:int(*-:{v:Int | true}*)) =
  if p > 0 && n < 0 then
    let bb = if nondet then 1 else 0 in
    f v bb p n
  else 0 
  
