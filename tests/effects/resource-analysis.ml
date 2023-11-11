let rec spend n =
  ev -1;
  if n <= 0 then 0 else spend (n - 1)

let main (gas:int(*-:{v:Int | true}*)) (n:int(*-:{v:Int | true}*)) = 
  if gas >= n then begin ev gas; spend n end else 0
