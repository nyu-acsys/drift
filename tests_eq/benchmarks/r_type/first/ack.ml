(*
USED: PLDI2011 as ack
USED: PEPM2013 as ack
*)


let rec ack m n =
  if m = 0 then n + 1
  else if n = 0 then ack (m - 1) 1
  else ack (m - 1) (ack m (n - 1))

let main mm mn =
    if (mm >= 0 && mn >= 0)
    then assert(ack mm mn >= mn)

let _ = main 3 2 