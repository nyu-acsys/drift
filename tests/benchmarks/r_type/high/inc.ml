
let rec ar (ari: int) = 0

let update (ua: int -> int) ui ux uj = if uj = ui then ux else ua uj

let rec g ge (ga: int -> int) gj =
 if gj < ge then
   (assert(0 <= gj && gj < ge);
    g ge (update ga gj (ga (gj) + 1)) (gj + 1))
 else ()

let main (n:int(*-:{v:Int | true}*)) =
	g n ar 0