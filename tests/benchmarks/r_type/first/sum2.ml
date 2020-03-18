
(*
Res: OCaml -> 10453878
imprecision: Oct: 4752 <= v
Liquid Haskell: TODO: need to TEST!
*)

let main (mn(*-:{v:Int | true}*)) = 
  let rec sum n =
    if n <= 0 then
      0
    else
      n + sum (n - 1)
  in

  assert (2 * mn - 1 <= sum mn) 
(* in
main 4752 *)
