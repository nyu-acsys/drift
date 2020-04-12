(*
USED: PLDI2011 as r-lock
USED: PEPM2013 as r-lock
*)

let main (mn(*-:{v:Int | true}*)) = 
    let lock st = assert (st = 0); 1 in
    let unlock st = assert (st = 1); 0 in
    let f n st = if n > 0 then lock (st) else st in
    let g n st = if n > 0 then unlock (st) else st in
    assert ((g mn (f mn 0)) = 0) 
(* in
main 10 *)
