let main r =
let rec bot bx = bot () in
let fail fx = assert (false) in

let rec fib_without_checking_1060 set_flag_fib_10520 s_fib_n_10490 n_10310 =
 let set_flag_fib_1052_r = true in
 let s_fib_n_1049_r = n_10310 in
   if n_10310 < 2 then
     1
   else
     fib_without_checking_1060 set_flag_fib_1052_r s_fib_n_1049_r (n_10310 - 1)
     +
     fib_without_checking_1060 set_flag_fib_1052_r s_fib_n_1049_r (n_10310 - 2)
in

let rec fib_1030 prev_set_flag_fib_1051 s_prev_fib_n_1050 n_1031 =
 let u = if prev_set_flag_fib_1051 then
          if (0 * 1) + (1 * s_prev_fib_n_1050) > (0 * 1) + (1 * n_1031) &&
             (0 * 1) + (1 * n_1031) >= 0 then
            ()
          else
            let u_1811 = fail ()
            in
              bot()
         else () in
        fib_without_checking_1060 prev_set_flag_fib_1051 s_prev_fib_n_1050
          n_1031
in


   let set_flag_fib_1052 = false in
   let s_fib_n_1049 = 0 in
   let res = fib_1030 set_flag_fib_1052 s_fib_n_1049 r in
   assert (res >= r)
in
main 30
