
let apply (af:int -> int) (ax:int) = af ax

let twice (tx:int) = 2 * tx

let neg_twice (nx:int) = 0 - (2 * nx)

let main (n:int(*-:{v:Int | true}*)) = 
    let res =
        if n >= 0 then
            apply twice n
        else 
            apply neg_twice n
    in
    assert(res >= 0)