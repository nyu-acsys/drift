let main =
let rec bot bx = bot () in
let fail fx = assert (false) in


let lambda_1031 set_flag_f_10891 s_f_x_10861 x_1033 = x_1033 + 1 in

let rec f_without_checking_1098 set_flag_f_1089 s_f_x_1086 x_1032 =
 let set_flag_f_1089_r = true
 in
 let s_f_x_1086_r = x_1032
 in
   if x_1032 > 0 then
     f_without_checking_1098 set_flag_f_1089_r s_f_x_1086_r (x_1032 - 1)
   else
     lambda_1031
in

let rec f_1030 prev_set_flag_f_1088 s_prev_f_x_1087 x_10320 =
 let u = if prev_set_flag_f_1088 then
          let u_2286 = fail ()
          in
            bot()
         else () in
        f_without_checking_1098 prev_set_flag_f_1088 s_prev_f_x_1087 x_10320
in

let g_1034 = f_1030 false 0 1 in

let main_1035 set_flag_f_1089m s_f_x_1086m u_1036 =
 g_1034 set_flag_f_1089m s_f_x_1086m 2
in


     main_1035 false 0 ()
in assert (main = 3)