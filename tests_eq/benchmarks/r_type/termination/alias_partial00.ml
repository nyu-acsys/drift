
let rec bot bx = bot ()
let fail fx = assert (false)

let rec lambda_without_checking_1078 set_flag_lambda_10638 s_lambda_x_10608 x_1033 =
 let set_flag_lambda_1063_r = true
 in
 let s_lambda_x_1060_r = x_1033
 in
   x_1033 + 1

let rec lambda_1031 prev_set_flag_lambda_1062 s_prev_lambda_x_1061 x_10331 =
 let u = if prev_set_flag_lambda_1062 then
          let u_1175 = fail ()
          in
            bot()
         else () in
        lambda_without_checking_1078 prev_set_flag_lambda_1062
          s_prev_lambda_x_1061 x_10331

let rec f_1030 set_flag_lambda_1063 s_lambda_x_1060 x_1032 =
 if x_1032 > 0 then
   f_1030 set_flag_lambda_1063 s_lambda_x_1060 (x_1032 - 1)
 else
   lambda_1031

let g_1034 = f_1030 false 0 1

let main_1035 set_flag_lambda_1063m s_lambda_x_1060m u_1036 =
 g_1034 set_flag_lambda_1063m s_lambda_x_1060m 2

let main =
 assert (main_1035 false 0 () = 3) 
