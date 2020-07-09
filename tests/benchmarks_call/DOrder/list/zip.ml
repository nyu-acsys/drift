let rec zip (xs:int list) (ys:int list) =
  match xs with
      [] -> (
        match ys with
              [] -> []
            | y::ys' -> assert(false) ; []
      )
    | x::xs' ->
        (match ys with
            [] -> assert(false) ; []
          | y::ys' -> (x, y)::(zip xs' ys'))

let rec make_list n =
  if n < 0
  then []
  else n :: make_list (n - 1)

let main (n:int) = 
  if n > 0 then 
    let xs = make_list n in
    let ys = zip xs xs in
    assert(List.length ys = List.length xs)
  else ()

let _ = main 1