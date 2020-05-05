
let lamp (x:unit) = x

let f =
  let id (dx:int -> unit) = dx in
  let unused = id (fun _ -> assert(false)) in
    lamp

let main (i:unit(*-:{v:Unit | unit}*))=
	f ()
