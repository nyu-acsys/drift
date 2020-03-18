

let rec cps_sum cn ck =
  if cn <= 0 then
    ck 0
  else
    let f fx = ck (fx + cn) in
    cps_sum (cn - 1) f

let main n =
    let u ux = assert (ux >= n) in
    cps_sum n u

let _ = main 100