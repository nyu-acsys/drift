
let rec loop lx ly ln = 
    if (ln < 10) then
        loop (lx + 2) (ly + 2) (ln + 1)
    else lx <> 4 || ly <> 0

let main_p (x:int) (y:int) (n:int) = 
    if (0 <= x && x <= 2 && 0 <= y && y <= 2 && 0 <= n) then
        assert(loop x y n = true)
    else ()

let main (w:unit) =
	let _ = main_p 0 0 4 in
    let _ = main_p 0 1 3 in
    let _ = main_p 1 0 0 in
    let _ = main_p 2 0 3 in
    let _ = main_p 4 0 5 in
    let _ = main_p 0 4 1 in
    let _ = main_p (-1) (-1) (-1) in
    (* let _ = 
    for i = 1 to 1000000 do
      main (Random.int 1000) (Random.int 1000) (Random.int 1000)
    done *)
	()

let _ = main ()