(*
USED: PLDI2011 as a-init
*)

let main (k(*-:{v:Int | true}*)) (n(*-:{v:Int | v >= 0}*)) (i(*-:{v:Int | v >= 0}*)) =
    let rec init idx tn ia =
      if idx >= tn then ia else (set ia idx 1; init (idx + 1) tn ia) 
    in

    let res =
        let x = init k n (make n 0) in
        if i < n then
            get x i >= 1
        else true
	in assert(res = true)
