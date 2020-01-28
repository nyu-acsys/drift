(*
USED: PLDI2011 as intro3
*)

let f x g = g (x + 1) in
let h z y = assert (y > z) in
let main n = if n >= 0 then f n (h n) else false in
main 1263