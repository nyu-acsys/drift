

let rec map x =
 if x = 0 then x else 1 + map (x - 1)

let main n =
    if n >= 0 then assert(map (map n) = n)

let _ = main 12329
