

let rec make_list n =
  if n <= 0
  then []
  else (fun m -> n + m) :: make_list (n - 1)

let rec fold_right (f:(int->int)->(int->int)->int->int) (xs:(int->int) list) (init:int->int) =
  match xs with
      [] -> init
    | x::xs' -> f x (fold_right f xs' init)

let compose (f:int->int) (g:int->int) (d:int) = f (g d)

let id (c:int) = c

let main (n:int) =
  if n > 0 then
    let xs = make_list n in
    let f = fold_right compose xs id in
    assert (f 0 >= 0)
  else ()

let _ = main 3