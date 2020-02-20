

let rec reduce rf rn ru = (* foldr *)
    if rn = 0 then ru
    else rf rn (reduce rf (rn - 1) ru)
in

let main n = 
	assert(accumulate (fun k t -> if t > 100 then 100 else t) n 0 <= 100) &&
	assert(accumulate (fun k t -> k + t) n 0 = (n + 1) * n / 2)
in main 20


(*
9 / 2 = bot?
assert(reduce (fun k t -> (k - k/2) = k/2) n true = ((n - n/2) = n/2))
*)