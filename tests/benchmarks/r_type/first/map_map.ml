let rec map x =
 if x = 0 then x else 1 + map (x - 1) in

let main n =
  assert(map (map n) = n) in
main 12329
