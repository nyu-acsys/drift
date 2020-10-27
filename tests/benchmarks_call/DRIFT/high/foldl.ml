
let rec accumulate (af:int -> int -> int) (an:int) (au:int) = (* foldl *)
    if an = 0 then au
    else accumulate af (an - 1) (af an au)

let rec accumulate_neg (nf:int -> bool -> bool) (nn:int) (nu:bool) = (* foldl *)
    if nn = 0 then nu
    else accumulate_neg nf (nn - 1) (nf nn nu)

let gt_100 (gk:int) (gt:int) = if gk > 100 then 100 else gt

let and_not_0 (ak:int) (at:bool) = at && ak <> 0

let main_p (n:int) =
    if n > 0 then
      assert(accumulate gt_100 n 100 = 100)
    else
      let m = 0 - n in
      assert(accumulate_neg and_not_0 m true = true)

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
