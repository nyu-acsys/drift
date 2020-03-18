
let main (i(*-:{v:Int | true}*))= 
	let lamp x = x in

	let f =
	  let id dx = dx in
	  let unused = id (fun _ -> assert(false)) in
	    lamp
	in

	f ()
(* in main 10 *)
(*
unit
*)
