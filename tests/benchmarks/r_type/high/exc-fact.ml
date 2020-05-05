(*
USED: PLDI2011 as e-fact
USED: PEPM2013 as e-fact
*)


let rec fact fn (exnx: int -> int) =
  if fn <= 0 then
    exnx 0
  else
    let exn1 k = if k = 0 then 1 else exnx k in
    fn * fact (fn - 1) exn1

let ex en = 0 - 1

let main (n:int(*-:{v:Int | true}*)) =
    if n > 0 then assert(fact n ex >= (0 - 1))
	else ()