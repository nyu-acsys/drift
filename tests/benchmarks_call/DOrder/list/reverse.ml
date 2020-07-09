
(*rename error*)
let rec reverseinner (xs:int list) (acc:int list) =
  match xs with
      [] -> acc
    | x :: xs' -> reverseinner xs' (x :: acc)

let reverse (xs:int list) = reverseinner xs []

let rec make_list n =
  if n = 0
  then []
  else n :: make_list (n - 1)

let hd (xs:int list) =
  match xs with
      [] -> -1
    | x::xs' -> x

let main (n:int) = 
  let xs = make_list n in
  if n > 0 then assert(hd (reverse xs) > 0)
  else ()

let _ = main 2