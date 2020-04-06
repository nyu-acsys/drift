(*
USED: PLDI2011 as ack
USED: PEPM2013 as ack
*)


let rec ack m n =
  if m = 0 then n + 1
  else if n = 0 then ack (m - 1) 1
  else ack (m - 1) (ack m (n - 1))

let main (mm:int) (mn:int) =
    if (mm >= 0 && mn >= 0)
    then assert(ack mm mn >= mn)
	else assert(true)

let _ = main 3 2
let _ = main 2 2
let _ = main 0 0
let _ = main (-1) (-1)