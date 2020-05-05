(*
From: SV-COMP 2020 half.c
*)

let rec half i k n = 
	if i >= 2 * k then n
	else 
		if i mod 2 = 0 then
			half (i + 1) k (n + 1)
		else half (i + 1) k n

let main_p (mn:int) = 
	if mn < 0 then ()
	else assert(half 0 mn 0 = mn)

let main (w:unit) =
	let _ = main_p 12 in
	let _ = main_p 540 in
(* let _ = 
    for i = 1 to 1000000 do
      main (Random.int 1000)
    done *)
	()

let _ = main ()