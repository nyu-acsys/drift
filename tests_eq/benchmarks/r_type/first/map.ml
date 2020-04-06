


let rec map x = (* x <= 3000 *)
  if x = 0
  then 0
  else 1 + map (x - 1) (* x < 0 && 0 < x <= 3000 *)

let main (n:int) =
    if n >= 0 then assert (map n = n)
    else assert(true)

let _ = main 3000
let _ = main 15
let _ = main 30
let _ = main (-43)
let _ = main 0
let _ = main (-3434)
