(*
USED: PLDI2011 as intro1
*)

let f x g = g (x + 1) in
let h y = assert (y > 0) in
let main n = if n > 0 then f n h else false in
main 120
