

let rec cps_sum (cn: int) (ck: int -> unit) =
  if cn <= 0 then
    ck 0
  else
    let f (fx: int) = ck (fx + cn) in
    cps_sum (cn - 1) f

let main_p (n:int) =
    let u (ux: int) = assert (ux >= n) in
    cps_sum n u

let main (w:unit) =
	let _ = main_p 100 in
(* let _ = 
    for i = 1 to 1000000 do
      main (Random.int 1000)
    done *)
	()

let _ = main ()