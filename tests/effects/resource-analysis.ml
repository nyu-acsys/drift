let rec spend n t =
  ev -1;
  if n <= 0 then 0 else spend (n-1) (t+1)

let main (gas:int(*-:{v:Int | true}*)) (n:int(*-:{v:Int | true}*)) = 
  if (gas > n && n >= 0) then begin ev gas; spend n 0 end else 0
