
let main n =
    let rec map x =
     if x = 0 then x else 1 + map (x - 1) in


    assert(map (map n) = n) in
main 12329
