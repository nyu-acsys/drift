(*
USED: PLDI2011 as intro1
*)

let main n = 
    let f x g = g (x + 1) in
    let h y = y > 0 in
    if n > 0 then f n h else false
in
assert (main 120 = true)
