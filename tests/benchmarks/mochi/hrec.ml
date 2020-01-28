(*
USED: PLDI2011 as hrec
*)

let rec f fg fx = if fx >= 0 then fg fx else f (f fg) (fg fx) in
let succ sx = sx + 1 in
let main n = assert (f succ n >= 0)
in main (-44)