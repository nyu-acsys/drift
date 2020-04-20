
let rec append x y =
  if x = 0 then
    y
  else
    1 + append (x - 1) y

let rec rev n =
  if n = 0
  then 0
  else append (rev (n - 1)) 1

let main (mn:int(*-:{v:Int | true}*)) (mm:int(*-:{v:Int | true}*)) =
  if mn >= 0 then assert (rev mn = mn)
	else assert(true)

let _ = main 3 5