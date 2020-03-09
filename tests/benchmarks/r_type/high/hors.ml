(*
USED: PLDI2011 as hors
USED: PEPM2013 as hors
*)

let main n = 
	let c cq = () in
	(* c q -> .  for any q *)
	let b bx bq = bx 1 in
	(* b q -> q1 for any q *)
	let a ax ay aq = if aq = 0 then (ax 0; ay 0) else (assert(false); ()) in
	(* a q0 -> q0 q0 *)

	let rec f fn fx fq = if fn <= 0 then fx fq else a fx (f (fn - 1) (b fx)) fq in
	(* F n x = if n<=0 then x else a x (f (n-1) (b x)) *)
	let s sn sq = f sn c sq in
	(* S -> F n c *)

	s n 0 
in
(* check whether S: q0 *)
main 30
