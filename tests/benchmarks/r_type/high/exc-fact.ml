(*
USED: PLDI2011 as e-fact
USED: PEPM2013 as e-fact
*)


let rec fact n exn =
  if n <= 0 then
    exn 0
  else
    let exnp n = if n = 0 then 1 else exn n in
    n * fact (n - 1) exnp

let exni n = assert false

let main (n:int(*-:{v:Int | true}*)) =
    if n > 0 then (fact n exni; ())