
let f x y = assert (x <= 0 || y > 0) in

let main x = f x x in

main 10
