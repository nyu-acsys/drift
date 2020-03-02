(*
USED: PLDI2011 as apply
*)

let apply f x = f x in
let g y z = assert (y = z) in
let rec k i n = if i >= n then true else (apply (g n) n && k (i+1) n) in
let main n = k 0 n in
main 20