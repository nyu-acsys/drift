

let f x y = assert (x <= 0 || y > 0)

let main (mx:int) = 
 	f mx mx

let _ = main 10
let _ = main 10
let _ = main 300
let _ = main 0
let _ = main (-34)
