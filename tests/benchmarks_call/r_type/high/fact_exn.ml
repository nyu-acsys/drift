(*
Sukyoung Ryu, Exception Analysis for Languages with Exceptions, Ph.D. thesis

let rec fact n =
  if n <= 0
  then raise ZERO
  else
    try
      n * fact (n-1)
    with ZERO -> 1
*)


let rec fact fn (exf: int -> int) =
  if fn <= 0
  then exf 0
  else
    let exn_n tn = if tn = 0 then 1 else exf tn in
      fn * (fact (fn - 1) exn_n)

let ex xn = 1

let main_p (n:int) =
  if n > 0
  then assert(fact n ex >= (-1))
  else ()

let main (w:unit) =
	let _ = main_p 300 in
(* let _ = 
    for i = 1 to 1000000 do
      main (Random.int 1000)
    done *)
	()

let _ = main ()