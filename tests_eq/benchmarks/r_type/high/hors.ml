(*
USED: PLDI2011 as hors
USED: PEPM2013 as hors
*)


let c cq = ()
(* c q -> .  for any q *)
let b (bx: int -> unit) bq = bx 1
(* b q -> q1 for any q *)
let a ax ay aq = if aq = 0 then (ax 0; ay 0) else ()
(* a q0 -> q0 q0 *)

let rec f fn (fx:int -> unit) fq = if fn <= 0 then fx fq else a fx (f (fn - 1) (b fx)) fq
(* F n x = if n<=0 then x else a x (f (n-1) (b x)) *)
let s sn sq = f sn c sq
(* S -> F n c *)

let main (n:int) = 
	s n 0 
(* check whether S: q0 *)
let _ = main 30
let _ = main 15
let _ = main 30
let _ = main (-43)
let _ = main 0
let _ = main (-3434)
