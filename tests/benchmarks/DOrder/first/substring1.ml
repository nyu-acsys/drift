(*
TODO: Should revise the predicate to allow:
k(*-:{v:Int | v >= 0 && k <= 31}*)
*)

let main (k(*-:{v:Int | v >= 0}*)) (toi(*-:{v:Int | v >= 0}*)) =   
	let rec loop lk li lj = 
	    if (lj < li) then
	        loop lk (li) (lj+1)
	    else lj < 31
	in

    if (k <= 30 && toi <= k) then
        (let i = toi in
        let j = 0 in
        assert(loop k i j = true))
    else assert(true)
(* in assert (main 20 5 = true) *)