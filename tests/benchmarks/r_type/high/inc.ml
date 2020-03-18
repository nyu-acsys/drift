

let main (n(*-:{v:Int | true}*)) =
	let rec ar ari = 0 in
	let update ua ui ux uj = if uj = ui then ux else ua uj in
	let rec g ge ga gj =
	 if gj < ge then
	   (assert(0 <= gj && gj < ge);
	    g ge (update ga gj (ga(gj) + 1)) (gj + 1))
	 else ()
	in
	
	g n ar 0
(* in main 30 *)
(* unit *)