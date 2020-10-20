
let apply (af:int -> int) (ax:int) = af ax

let twice (tx:int) = 2 * tx

let neg_twice (nx:int) = 0 - (2 * nx)

let main_p (n:int) = 
    let res =
        if n >= 0 then
            apply twice n
        else 
            apply neg_twice n
    in
    assert(res >= 0)

let main (w:unit) =
	let _ = main_p 4  in
    let _ = main_p 0 in
    let _ = main_p (-34) in
(* let _ = 
    for i = 1 to 1000000 do
      main (Random.int 1000)
    done *)
	()

let _ = main ()