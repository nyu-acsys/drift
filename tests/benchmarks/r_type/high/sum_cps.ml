

let rec cps_sum (cn: int) (ck: int -> unit) =
  if cn <= 0 then
    ck 0
  else
    let f (fx: int) = ck (fx + cn) in
    cps_sum (cn - 1) f

let main (n:int(*-:{v:Int | true}*)) =
    let u (ux: int) = assert (ux >= n) in
    cps_sum n u

let _ = main 100