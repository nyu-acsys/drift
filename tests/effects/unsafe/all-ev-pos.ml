(* *** Based on COAR: **********************************************************
let rec sum x = if x <= 0 then 0 else x + sum (x - 1)
[@@@assert "typeof(sum) <: (x:{ x:int | true }) -> { ret : int | ret > x }"]
[@@@assert "typeof(sum) <: (x:{ x:int | x > 1 }) -> { ret : int | ret > x }"]
[@@@assert "typeof(sum) <: (x:{ x:int | true }) -> { ret : int | ret >= x }"]
****************************************************************************** *)

(* Properties: prop-all-ev-pos.eff *)

let rec sum x = 
  ev (x-1);
  if x <= 0 then 0 else x + (sum (x - 1))

let main (v:int(*-:{v:Int | true}*)) =
  if v < 0 then sum (0 - v) else sum v  
