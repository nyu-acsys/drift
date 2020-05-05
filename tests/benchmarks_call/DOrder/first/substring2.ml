

let rec loop lk li lj = 
    if (li < lk) then
        loop lk (li+1) (lj+1)
    else lj < 31

let main_p (k:int) (from:int) =   
    if (k >= 0 && k <= 30 && from >= 0 && from <= k) then
        (let i = from in
        let j = 0 in
        assert (loop k i j = true))
    else ()

let main (w:unit) =
    let _ = main_p 20 5 in
    let _ = main_p 9 0 in
    let _ = main_p 0 9 in
    let _ = main_p 5 20 in
    let _ = main_p 60 60 in
    let _ = main_p (-1) (-1) in
(* let _ = 
    for i = 1 to 1000000 do
      main (Random.int 1000) (Random.int 1000)
    done *)
	()

let _ = main ()