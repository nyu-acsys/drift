(*
Res: OCaml -> 284635
imprecision: Oct: 754 <= v
Liquid Haskell: TODO: need to TEST!
*)

let rec sum n =
  if n <= 0 then
    0
  else
    n + sum (n - 1)
in
let main n = assert (4 * n - 6 <= sum n) in
main 754

(* Reason: wid overapproximate *)