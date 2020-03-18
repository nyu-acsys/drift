
(*
Res: OCaml -> 91
imprecision: Oct: 91 <= v
Liquid Haskell: TODO: need to TEST!
*)

let main (n(*-:{v:Int | true}*)) =
    let rec m mx mk =
      if mx > 100
      then mk (mx - 10)
      else
        let f fr = m fr mk in
        m (mx + 11) f
    in

    let k kr = kr = 91 in
    assert (m n k = true)
(* in
assert (main 22 = true) *)
