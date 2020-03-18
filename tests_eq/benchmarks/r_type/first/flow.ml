


let lamp x = x

let f =
  let id dx = dx in
  let unused = id (fun _ -> assert(false)) in
    lamp

let main i = 
	f ()

let _ = main 10
(*
unit
*)
