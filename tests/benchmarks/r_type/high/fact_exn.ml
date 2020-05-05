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

let main (n:int(*-:{v:Int | true}*)) =
  if n > 0
  then assert(fact n ex >= (-1))
  else ()