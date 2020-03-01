
let app ab af aa ag = af aa ag
in

let f fx fa fk = fk fx
in

let check x y = assert (x = y)
in

let main a b = app (4 * a + 2 * b) (f (4 * a + 2 * b)) (4 * a + 2 * b) (check (4 * a + 2 * b))
in
main 4 5