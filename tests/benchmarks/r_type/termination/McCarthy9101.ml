
let rec bot bx = bot () in
let fail fx = assert (false) in

let rec mc91_without_checking_1058 set_flag_mc91_10508 s_mc91_n_10478 n_10318 =
 let set_flag_mc91_1050_r = true
 in
 let s_mc91_n_1047_r = n_10318
 in
   if n_10318 > 100 then
     n_10318 - 10
   else
     mc91_without_checking_1058 set_flag_mc91_1050_r s_mc91_n_1047_r
       (mc91_without_checking_1058 set_flag_mc91_1050_r s_mc91_n_1047_r
         (n_10318 + 11))
in

let rec mc91_1030 prev_set_flag_mc91_1049 s_prev_mc91_n_1048 n_1031 =
 let u = if prev_set_flag_mc91_1049 then
          if (111 * 1) + (0-s_prev_mc91_n_1048) > (111 * 1) + (0-n_1031) &&
             (111 * 1) + (0-n_1031) >= 0 then
            ()
          else
            let u_1771 = fail ()
            in
              bot()
         else () in
        mc91_without_checking_1058 prev_set_flag_mc91_1049
          s_prev_mc91_n_1048 n_1031
in

let main r =
 let set_flag_mc91_1050 = false in
 let s_mc91_n_1047 = 0 in
 mc91_1030 set_flag_mc91_1050 s_mc91_n_1047 r
in assert(main (-49) >= 91)
