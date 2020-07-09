
let rec make_list n =
  if n <= 0
  then []
  else n :: make_list (n - 1)

let rec length (xs:int list) =
  match xs with
      [] -> 0
    | x::xs' -> 1 + length xs'

let main (n:unit) =
  let xs = [0; 1; 2] in
  let ys = make_list 3 in
  assert(length xs = length ys)

let _ = main ()