
let rec reduce n =
  ev -1;
  if n <= 0 then 0 else begin reduce (n - 1) end

let main (mn:int(*-:{v:Int | true}*)) = 
  if mn > 0 then begin ev mn; reduce (mn-1) end else 0
