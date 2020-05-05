
(*
Res: OCaml -> 91
imprecision: Oct: 91 <= v
Liquid Haskell: TODO: need to TEST!
*)


let rec m mx (mk:int -> bool): bool =
  if mx > 100
  then true && mk (mx - 10)
  else
    let f (fr:int) = m fr mk in
    m (mx + 11) f

let main (n:int(*-:{v:Int | true}*)) =
	if n <= 101 then
	    (let k kr = (kr = 91) in
	    assert (m n k = true))
	else ()
