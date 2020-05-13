

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

let main (n:int(*-:{v:Int | true}*)) =
    if n >= 0 then
        assert(reduce gt_5 n 5 = 5)
    else
        let m = 0 - n in
        assert(reduce_neg exor m true = true)

let _ = main 20
let _ = main 30
let _ = main 3098
let _ = main 0
let _ = main (-1)
let _ = main (-20)
let _ = main (-304)