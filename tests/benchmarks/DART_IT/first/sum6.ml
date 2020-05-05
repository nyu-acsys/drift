

let rec sum n =
  if n <= 0
  then 0
  else n + sum (n - 1)

let main (mn:int(*-:{v:Int | true}*)) =
    assert (6 * mn - 15 <= sum mn)

(* let _ = 
    for i = 1 to 1000 do
      main (Random.int 1000)
    done *)