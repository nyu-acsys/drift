(*
From: SV-COMP 2020 half.c
*)

let rec half i k n = 
	if i >= 2 * k then n
	else 
		if i mod 2 = 0 then
			half (i + 1) k (n + 1)
		else half (i + 1) k n

let main (mn:int(*-:{v:Int | true}*)) = 
	if mn < 0 then ()
	else assert(half 0 mn 0 = mn)

(* let _ = 
    for i = 1 to 10000 do
      main (Random.int 10000)
    done *)