

let rec map x =
 if x = 0 then x else 1 + map (x - 1)

let main (n:int(*-:{v:Int | true}*)) =
    if n >= 0 then assert(map (map n) = n)
    else ()