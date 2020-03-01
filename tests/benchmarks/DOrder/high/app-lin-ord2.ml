
let app ab af ax = af ax 
in

let check cx cy = assert (cx = cy) 
in

let main a b = app (4 * a + 2 * b) (check (4 * a + 2 * b)) (4 * a + 2 * b) 
in
main 5 3