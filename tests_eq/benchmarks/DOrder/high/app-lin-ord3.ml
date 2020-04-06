

let app (ab:int) (af:int->(int->unit)->unit) aa (ag:int->unit) = af aa ag

let f (fx:int) (fa:int) (fk:int->unit) = fk fx

let check (x:int) (y:int) = assert (x = y)

let main (a:int) (b:int) = 
    app (4 * a + 2 * b) (f (4 * a + 2 * b)) (4 * a + 2 * b) (check (4 * a + 2 * b))

let _ = main 4 5
let _ = main 75 34
let _ = main 0 8
let _ = main 100 12
let _ = main (-53) 23
let _ = main 8 (-573)