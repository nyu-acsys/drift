(*
USED: PLDI2011 as apply
*)

let apply f x = f x in
let g y z = assert (y = z) in
let main n = apply (g n) n in
main 20
