let is_nil (xs:int list) =
  match xs with
      [] -> 1
    | _ -> 0

let rec make_list n =
  if n = 0
  then []
  else n :: make_list (n - 1)

let main (n:int(*-:{v:Int | true}*)) =
  if n > 0
  then 
    let xs = make_list n in
    assert((is_nil xs) <> 1)
  else ()