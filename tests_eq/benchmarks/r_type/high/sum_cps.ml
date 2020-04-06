

let rec cps_sum (cn: int) (ck: int -> unit) =
  if cn <= 0 then
    ck 0
  else
    let f (fx: int) = ck (fx + cn) in
    cps_sum (cn - 1) f

let main (n:int) =
    let u (ux: int) = assert (ux >= n) in
    cps_sum n u

let _ = main 100
let _ = main 0
let _ = main 15
let _ = main 30
let _ = main (-43)
let _ = main 0
let _ = main (-3434)