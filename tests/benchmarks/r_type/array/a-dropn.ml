(*
Drop every i'th element from origin array
*)

let make_array (an:int) (ai:int) = assert (0 <= ai && ai < an); 0
let update (ui:int) (un:int) (ua:int -> int) (ux:int) = 
	ua ui;
  	let ap pj = assert (0 <= ui && ui < un); if ui = pj then ux else ua pj in ap

let rec drop cai can cbi cbn cdi cdn (ca:int -> int) (cb:int -> int) = 
	if cai < can && cbi < cbn then 
		if cdi < cdn then
			(let cbp = update cbi cbn cb (ca cai) in
			drop (cai + 1) can (cbi + 1) cbn (cdi + 1) cdn ca cbp)
		else drop (cai + 1) can cbi cbn 0 cdn ca cb
	else cb

let main (mn:unit) = 
	let n = 30 in
	let i = 4 in
	let a = make_array n in
	let j = n - 7 in
	let b = make_array j in
	let bp = drop 0 n 0 j 0 i a b in
	bp(0); bp(j - 1); ()

let _ = main ()