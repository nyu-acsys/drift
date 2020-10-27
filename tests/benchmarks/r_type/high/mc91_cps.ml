
(*
Res: OCaml -> 91
imprecision: Oct: 91 <= v
Liquid Haskell: TODO: need to TEST!
*)


let rec m x (k:int -> unit): unit =
  if x > 100
  then k (x-10)
  else
    let f r = m r k in
    m (x+11) f

let main (n:int(*-:{v:Int | true}*)) =
	let k r = if n <= 101 then assert (r = 91) in
  	m n k
