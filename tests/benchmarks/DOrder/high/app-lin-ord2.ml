
let app (ab:int) (af:int->unit) ax = af ax 

let check (cx:int) (cy:int) = assert (cx = cy) 

let main (a:int(*-:{v:Int | true}*)) (b:int(*-:{v:Int | true}*)) =
    app (4 * a + 2 * b) (check (4 * a + 2 * b)) (4 * a + 2 * b) 