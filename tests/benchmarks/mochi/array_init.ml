

let rec init i n a =
  if i>=n then a
  else (set a i 1; init (i+1) n a)
in

let main n i =
  let x = init 0 n (make n 0) in
   if 0<=i && i<n then
    assert (get x i >=i) (* check that the array has been initialized *)
  else false
in main 3 2