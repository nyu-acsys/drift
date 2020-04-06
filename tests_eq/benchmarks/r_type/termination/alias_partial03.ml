
let rec bot bx = bot ()
let fail fx = assert (false)


let lambda_1031 set_flag_f_10891 s_f_x_10861 x_1033 = x_1033 + 1

let rec f_without_checking_1098 set_flag_f_1089 s_f_x_1086 x_1032 =
 let set_flag_f_1089_r = true
 in
 let s_f_x_1086_r = x_1032
 in
   if x_1032 > 0 then
     f_without_checking_1098 set_flag_f_1089_r s_f_x_1086_r (x_1032 - 1)
   else
     lambda_1031

let rec f_1030 f_without_checking_10980 prev_set_flag_f_1088 s_prev_f_x_1087 x_10320 =
 let u =
 if prev_set_flag_f_1088 then
          if (0 * 1) + (1 * s_prev_f_x_1087) > (0 * 1) + (1 * x_10320) &&
             (0 * 1) + (1 * x_10320) >= 0 then
            ()
          else
            let u_4274 = fail ()
            in
              bot()
 else
   ()
     in
        f_without_checking_10980 prev_set_flag_f_1088 s_prev_f_x_1087 x_10320

let g_1034 = f_without_checking_1098 false 0 1

let main_1035 set_flag_f_1089m s_f_x_1086m u_1036 =
 g_1034 set_flag_f_1089m s_f_x_1086m 2

let main =
 assert (main_1035 false 0 () = 3) 
