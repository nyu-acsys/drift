(*
    USED: PEPM2013 as copy_intro
*)

let main n =
    let rec copy x = 
        if x = 0 then 0 
        else 1 + copy (x - 1) 
    in
    assert (copy (copy n) = n) 
in
main 1823
