
let sum x = 
  ev 1; (fun y -> x + y)

let main (u: unit(*-:{v:Unit | unit}*)) = 
  ev 2;
  ev (sum (ev 3; 10) (ev 4; 20))
