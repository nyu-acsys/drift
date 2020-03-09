(*
USED: PLDI2011 as e-fact
USED: PEPM2013 as e-fact
*)

let main n = 
    let rec fact fn exnx =
      if fn <= 0 then
        exnx 0
      else
        let exn1 k = if k = 0 then 1 else exnx k in
        fn * fact (fn - 1) exn1
    in
    let exn en = -1 in
    if n > 0 then fact n exn >= (-1) else false 
in assert(main 342 = true)