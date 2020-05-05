
let rec ar (ari: int) = 0

let update (ua: int -> int) ui ux uj = if uj = ui then ux else ua uj

let rec g ge (ga: int -> int) gj =
 if gj < ge then
   (assert(0 <= gj && gj < ge);
    g ge (update ga gj (ga (gj) + 1)) (gj + 1))
 else ()

let main_p (n:int) =
	g n ar 0

let main (w:unit) =
	let _ = main_p 30 in
(* let _ = 
    for i = 1 to 1000000 do
      main (Random.int 1000)
    done *)
	()

let _ = main ()
(* unit *)