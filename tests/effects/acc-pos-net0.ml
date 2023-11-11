
let rec reduce n =
  ev -1;
  if n <= 0 then 0 else reduce (n - 1)

let main (n:int(*-:{v:Int | true}*)) = 
  if n > 0 then begin ev n; reduce n else 0
