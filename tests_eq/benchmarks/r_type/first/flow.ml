


let lamp (x:unit) = x

let f =
  let id (dx:int -> unit) = dx in
  let unused = id (fun _ -> assert(false)) in
    lamp

let main (i:int) = 
	f ()

let _ = main 10
(*
unit
*)
