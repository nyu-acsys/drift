(*
Res: OCaml -> 284635
imprecision: Oct: 754 <= v
Liquid Haskell: TODO: need to TEST!
*)
let main (mn(*-:{v:Int | true}*)) = 
    let rec sum n =
      if n <= 0 then
        0
      else
        n + sum (n - 1)
    in
    assert (4 * mn - 6 <= sum mn) 
(* in
main 754 *)
