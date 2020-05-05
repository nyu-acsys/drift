

let rec map x = (* x <= 3000 *)
  if x = 0
  then 0
  else 1 + map (x - 1) (* x < 0 && 0 < x <= 3000 *)

let main (n:int(*-:{v:Int | true}*)) =
    if n >= 0 then assert (map n = n)
    else ()
