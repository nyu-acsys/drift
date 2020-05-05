

let app (ab:int) (af:int->(int->unit)->unit) aa (ag:int->unit) = af aa ag

let f (fx:int) (fa:int) (fk:int->unit) = fk fx

let check (x:int) (y:int) = assert (x = y)

let main (a:int(*-:{v:Int | true}*)) (b:int(*-:{v:Int | true}*)) =
    app (4 * a + 2 * b) (f (4 * a + 2 * b)) (4 * a + 2 * b) (check (4 * a + 2 * b))
