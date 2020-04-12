(*
USED: PLDI2011 as e-fact
USED: PEPM2013 as e-fact
*)

let main (n(*-:{v:Int | v > 0}*)) = 
    let rec fact fn exnx =
      if fn <= 0 then
        exnx 0
      else
        let exn1 k = if k = 0 then 1 else exnx k in
        fn * fact (fn - 1) exn1
    in
    let ex en = 0 - 1 in
    assert(fact n ex >= (0 - 1))
(* in assert(main 342 = true) *)