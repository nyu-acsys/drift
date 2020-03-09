(*
USED: PLDI2011 as a-init
*)

let main k n i =
    let rec init idx tn ia =
      if idx >= tn then ia else (set ia idx 1; init (idx + 1) tn ia) 
    in

    if k >= 0 then
        let x = init k n (make n 0) in
        if 0 <= i && i < n then
            get x i >= 1
        else false
    else false
in
main 0 3 2
