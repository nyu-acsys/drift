
let rec mono t x =
  ev t;
  if x <= 0 then 0
  else
    mono (t+x) (x-1) (* increase t by positive amount *)

let main (u: int(*-:{v:Int | true}*)) = 
  if u>0 then mono 1 u else 0

