

let f x y = assert (x <= 0 || y > 0)

let main mx = 
 	f mx mx

let _ = main 10

