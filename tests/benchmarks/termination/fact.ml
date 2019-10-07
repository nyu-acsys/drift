
let rec fact x = if x <= 0 then 1 else x * (fact (x - 1)) in
fact 2

(*
((lambda fact^0. (fact^1 3^2)^3)^4
  (mu fact^5 x^6.
     (
     	(x^7 <= 0^8)^9 ? 1^10 : (x^11 * (fact^12 (x^13 - 1^14)^15)^16)^17
     )^18
  )^19
)^20
*)