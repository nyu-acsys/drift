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

let main (n(*-:{v:Int | v > 0}*)) =
  let rec fact fn exf =
    if fn <= 0
    then exf 0
    else
      let exn_n tn = if tn = 0 then 1 else exf tn in
        fn * (fact (fn - 1) exn_n)
  in

  let ex xn = assert(false); 1 in

  assert(fact n ex >= (-1))
(* in assert(main 300 = true) *)
