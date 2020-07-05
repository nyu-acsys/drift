let rec zip xs ys =
  match xs with
      [] -> (match ys with
              [] -> []
            | y::ys' -> assert(false); [])
    | x::xs' -> (match ys with
            [] -> assert(false) ; []
          | y::ys' -> (x,y) ::zip xs' ys')

let rec unzip xs =
  match xs with
      [] -> [], []
    | (a,b)::xs' ->
       let (ys, zs) = unzip xs' in
         a::ys, b::zs

let rec make_list n =
  if n < 0
  then []
  else (n, n) :: make_list (n - 1)

let main (n:int(*-:{v:Int | true}*)) =
  if n > 0 then
    let xs = make_list n in
    let (ys,zs) = unzip xs in
    let xys = zip ys zs in
    ()
  else ()