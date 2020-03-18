


let rec ar ari = 0

let update ua ui ux uj = if uj = ui then ux else ua uj

let rec g ge ga gj =
 if gj < ge then
   (assert(0 <= gj && gj < ge);
    g ge (update ga gj (ga(gj) + 1)) (gj + 1))
 else ()

let main n =
	g n ar 0

let _ = main 30
(* unit *)