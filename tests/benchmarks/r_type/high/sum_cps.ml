
let main (n(*-:{v:Int | true}*)) =
    let rec cps_sum cn ck =
      if cn <= 0 then
        ck 0
      else
        let f fx = ck (fx + cn) in
        cps_sum (cn - 1) f
    in

    let u ux = assert (ux >= n) in
    cps_sum n u
(* in
main 100 *)