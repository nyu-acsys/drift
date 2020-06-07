
let apply af ax = af ax

let twice tx = 2 * tx

let neg_twice nx = 0 - (2 * nx)

let main (n(*-:{v:Int | true}*)) = 
    let res =
        (if n >= 0 then
            apply twice 
        else 
            apply neg_twice) n
    in
    assert(res >= 0)