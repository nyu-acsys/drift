
let rec reduce n =
  ev -1;
  if n <= 0 then 0 else reduce (n - 1)

let main (mn:int(*-:{v:Int | true}*)) = 
  if mn > 0 then begin ev mn; reduce mn end else begin ev 1; 0 end
