(*
USED: PLDI2011 as e-fact
USED: PEPM2013 as e-fact
*)

let rec fact fn exnx =
  if fn <= 0 then
    exnx 0
  else
    let exn1 k = if k = 0 then 1 else exnx k in
    fn * fact (fn - 1) exn1
in
let exn en = -1 in
let main n = if n > 0 then assert(fact n exn >= (-1)) else false 
in main 342