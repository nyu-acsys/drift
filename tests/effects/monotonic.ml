
let rec mono x t =
  ev t;
  if x <= 0 then 0
  else
    mono (x-1) (t+x) (* increase t by positive amount *)

let main (u: unit(*-:{v:Unit | unit}*)) = 
  if u>0 then mono u 1 else 0

