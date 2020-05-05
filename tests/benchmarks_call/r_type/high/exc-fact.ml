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

let main_p (n:int) =
    if n > 0 then assert(fact n ex >= (0 - 1))
	else ()

let main (w:unit) =
	let _ = main_p 342 in
(* let _ = 
    for i = 1 to 1000000 do
      main (Random.int 1000)
    done *)
	()

let _ = main ()