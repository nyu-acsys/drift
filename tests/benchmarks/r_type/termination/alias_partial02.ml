
let rec bot _ = bot ()
let fail _ = assert false


   let lambda_1031 set_flag_f_1089 s_f_x_1086 x_1033 = x_1033 + 1

   let rec f_without_checking_1098 set_flag_f_1089 s_f_x_1086 x_1032 =
     let set_flag_f_1089 = true
     in
     let s_f_x_1086 = x_1032
     in
       if x_1032 > 0 then
         f_without_checking_1098 set_flag_f_1089 s_f_x_1086 (x_1032 - 1)
       else
         lambda_1031

   let rec f_1030 prev_set_flag_f_1088 s_prev_f_x_1087 x_1032 =
     let u =     if prev_set_flag_f_1088 then
              if (0 * 1) + (1 * s_prev_f_x_1087) > (0 * 1) + (1 * x_1032) &&
                 (0 * 1) + (1 * x_1032) >= 0 then
                ()
              else
                let u_3528 = fail ()
                in
                  bot()
     else () in
            f_without_checking_1098 prev_set_flag_f_1088 s_prev_f_x_1087 x_1032

   let g_1034 = f_1030 false 0 1

   let main_1035 set_flag_f_1089 s_f_x_1086 u_1036 =
     g_1034 set_flag_f_1089 s_f_x_1086 2

let main =
     main_1035 false 0 ()
