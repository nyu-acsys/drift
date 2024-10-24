

let rec reduce (rf:int -> int -> int) (rn:int) (ru:int) = (* foldr *)
    if rn = 0 then ru
    else rf rn (reduce rf (rn - 1) ru)

let rec reduce_neg (nf:int -> bool -> bool) (nn:int) (nu:bool) = (* foldr *)
    if nn = 0 then nu
    else nf nn (reduce_neg nf (nn - 1) nu)

let gt_5 (gk:int) (gt:int) = if gk > 5 then 5 else gt

let not_bool (nx:bool) = if nx then false else true

let exor (ak:int) (at:bool) = (*at will always be true*)
    let pk = not_bool at
    in (pk || at) && (not_bool(pk && at)) (*XOR: given one ture will be true*)

let main_p (n:int) =
    if n >= 0 then
        assert(reduce gt_5 n 5 = 5)
    else
        let m = 0 - n in
        assert(reduce_neg exor m true = true)

let main (w:unit) =
	let _ = main_p 20 in
    let _ = main_p 30 in
    let _ = main_p (-20) in
(* let _ = 
    for i = 1 to 1000000 do
      main (Random.int 1000)
    done *)
	()

let _ = main ()

