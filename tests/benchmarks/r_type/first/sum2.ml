
(*
Res: OCaml -> 10453878
imprecision: Oct: 4752 <= v
Liquid Haskell: TODO: need to TEST!
*)

let rec sum n =
  if n <= 0 then
    0
  else
    n + sum (n - 1)
in
let main n = assert (2 * n - 1 <= sum n) in
main 4752

(* Reason: wid overapproximate *)