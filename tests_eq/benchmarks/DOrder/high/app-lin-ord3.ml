

let app ab af aa ag = af aa ag

let f fx fa fk = fk fx

let check x y = assert (x = y)

let main a b = 
    app (4 * a + 2 * b) (f (4 * a + 2 * b)) (4 * a + 2 * b) (check (4 * a + 2 * b))

let _ = main 4 5