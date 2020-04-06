(*
TODO: Should revise the predicate to allow:
k(*-:{v:Int | v >= 0 && k <= 30}*)
*)


let main (k(*-:{v:Int | v >= 0}*)) (from(*-:{v:Int | v >= 0}*)) = 
    let rec loop lk li lj = 
        if (li < lk) then
            loop lk (li+1) (lj+1)
        else lj < 31
    in
  
    if (k <= 30 && from <= k) then
        (let i = from in
        let j = 0 in
        assert(loop k i j = true))
    else assert(true)
(* in assert (main 5 20 = true) *)