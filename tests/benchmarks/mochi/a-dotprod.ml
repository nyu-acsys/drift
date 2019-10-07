(*
USED: PLDI2011 as a-prod
*)

let rec dotprod n v1 v2 i sum =
  if i >= n then
    sum
  else
    dotprod n v1 v2 (i + 1) (sum + (get v1 i) * (get v2 i))
in
let main z n =
  let v1 = make n 0 in
  let v2 = make n 1 in
  if z = 0 then dotprod n v1 v2 z z
  else -1
in main 0 3
