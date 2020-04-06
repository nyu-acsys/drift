


let app (ab:int) (af:int->unit) ax = af ax 

let check (cx:int) (cy:int) = assert (cx = cy) 

let main (a:int) (b:int) = 
    app (4 * a + 2 * b) (check (4 * a + 2 * b)) (4 * a + 2 * b) 

let _ = main 4 5
let _ = main 27 37
let _ = main 0 75
let _ = main 7 2
let _ = main (-37) 75
let _ = main 75 (-27)