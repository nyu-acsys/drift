
(*
Res: OCaml -> 91
imprecision: Oct: 91 <= v
Liquid Haskell: TODO: need to TEST!
*)

let rec m mx mk =
  if mx > 100
  then mk (mx - 10)
  else
    let f fr = m fr mk in
    m (mx + 11) f
in
let main n =
  let k kr = if n <= 101 then assert (kr = 91) else false in
  m n k
in
main 22
