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

let ex en = 0 - 1

let main (n:int) = 
    if n > 0 then assert(fact n ex >= (0 - 1))
	else assert(true)

let _ = main 342
let _ = main 15
let _ = main 30
let _ = main (-43)
let _ = main 0
let _ = main (-3434)