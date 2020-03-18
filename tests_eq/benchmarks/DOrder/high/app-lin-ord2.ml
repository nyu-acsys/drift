


let app ab af ax = af ax 

let check cx cy = assert (cx = cy) 

let main a b = 
    app (4 * a + 2 * b) (check (4 * a + 2 * b)) (4 * a + 2 * b) 

let _ = main 5 3