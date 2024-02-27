let rec f x pos neg =
  if x > 0 then
    ev pos
  else if x < 0 then  
    ev neg
  else 
    ();

  if x > 0 then
    f (x-1) pos neg
  else 
    (if x < 0 then 
       f (x+1) pos neg
     else
       0)
  
let main (v:int(*-:{v:Int | true}*)) (p:int(*-:{v:Int | true}*)) (n:int(*-:{v:Int | true}*)) =
  if p > 0 && n < 0 then
    f v p n
  else 0 
    

(* let rec f x pos neg = 
  if x >= 0 then f (x-1) pos neg
  else f (x+1) pos neg

let main (pos1:int(*-:{v:Int | true}*)) (neg1:int(*-:{v:Int | true}*)) = f 100 pos1 neg1 *)