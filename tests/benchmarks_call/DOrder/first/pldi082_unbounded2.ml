
let rec loop (lx:int) (ly:int) (ln:int) = 
        let ny = 
            if (lx <= ln) then ly + 1 
            else if (lx >= ln+1) then ly - 1 
            else (-4) in
        if (ny < 0) then 
            if ln >= 0 then 
                if ny = (-1) then (lx < 2 * ln + 3)
                else false
            else false
        else loop (lx+1) ny ln 

let main_p (n:int) =
    let x:int = 0 in
    let y:int = 0 in
    if(n < 0) then
        assert(loop x y n = false)
    else ()

let main (w:unit) =
	let _ = main_p (-3) in
    let _ = main_p 7 in
    let _ = main_p 0 in
    let _ = main_p (-49) in
(* let _ = 
    for i = 1 to 1000000 do
      main (Random.int 1000)
    done *)
	()

let _ = main ()