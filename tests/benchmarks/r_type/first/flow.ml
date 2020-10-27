
let lamp (x:unit) = x

let f =
  let id (x:int -> unit) = x in
  let unused = id (fun _ -> assert false) in
    lamp

let main (i:unit(*-:{v:Unit | unit}*))=
	f ()
