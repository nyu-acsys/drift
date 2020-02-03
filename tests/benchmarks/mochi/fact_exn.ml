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

let rec fact fn exn =
  if fn <= 0
  then exn 0
  else
    let exn n = if n = 0 then 1 else exn n in
      fn * (fact (fn - 1) exn)
in

let exn xn = assert(false); 1 in

let main n =
  if n > 0
  then assert(fact n exn >= (-1))
  else false
in main 300
