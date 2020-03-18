(*
Res: OCaml -> 42813631
imprecision: Oct: 9253 <= v; wid overapproximate
Liquid Haskell: TODO: need to TEST!
*)
let main (mn(*-:{v:Int | true}*)) = 
    let rec sum n =
      if n <= 0 then
        0
      else
        n + sum (n - 1)
    in
    assert (3 * mn - 3 <= sum mn) 
(* in
main 9253 *)
