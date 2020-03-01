let lamp x = x in

let f =
  let id dx = dx in
  let unused = id (fun _ -> assert(false)) in
    lamp
in

let main i = f ()
in main 10
(*
unit
*)
