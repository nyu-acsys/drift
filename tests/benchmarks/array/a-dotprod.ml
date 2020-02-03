(*
USED: PLDI2011 as a-prod
*)

let rec dotprod n vd1 vd2 i sum =
  if i >= n then
    sum
  else
    dotprod n vd1 vd2 (i + 1) (sum + (get vd1 i) * (get vd2 i))
in
let main z n =
  let v1 = make n 0 in
  let v2 = make n 1 in
  if z = 0 then assert (dotprod n v1 v2 z z = 0)
  else false
in main 0 3
