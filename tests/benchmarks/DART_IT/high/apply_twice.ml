
let main (n(*-:{v:Int | true}*)) = 
	let apply af ax = af ax in

	let twice tx = 2 * tx in

	let neg_twice nx = 0 - (2 * nx) in

    let res =
        if n >= 0 then
            apply twice n
        else 
            apply neg_twice n
    in
    assert(res >= 0)


(* let _ = main 3
let _ = main 10
let _ = main 0
let _ = main (-34)
let _ = main (-203) *)