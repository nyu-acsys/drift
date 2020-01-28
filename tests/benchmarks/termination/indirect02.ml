
let rec bot bx = bot () in
let fail fx = assert (false) in

let c1_COEFFICIENT_1085 = 0 in
let c0_COEFFICIENT_1084 = 0 in

let id_1030 set_flag_f_11910 s_f_x_11880 x_10310 = x_10310 in

let app_1032 x_DO_NOT_CARE_1222 x_DO_NOT_CARE_1223 h_EXPARAM_1087 x_DO_NOT_CARE_1220 x_DO_NOT_CARE_1221 h_1033 x_DO_NOT_CARE_1218 x_DO_NOT_CARE_1219 v_1034 set_flag_f_11912 s_f_x_11882 u_1035 =
 h_1033 set_flag_f_11912 s_f_x_11882 v_1034 set_flag_f_11912 s_f_x_11882 u_1035
in

let rec f_without_checking_1199 set_flag_f_1191 s_f_x_1188 x_1037 =
 let set_flag_f_1191_r = true
 in
 let s_f_x_1188_r = x_1037
 in
   if x_1037 > 0 then
     app_1032 set_flag_f_1191_r s_f_x_1188_r
       ((c0_COEFFICIENT_1084 * x_1037) + c1_COEFFICIENT_1085)
       set_flag_f_1191_r s_f_x_1188_r f_without_checking_1199 set_flag_f_1191_r
       s_f_x_1188_r (x_1037 - 1)
   else
     id_1030
in

let rec f_1036 prev_set_flag_f_1190 s_prev_f_x_1189 x_10376 =
 let u = if prev_set_flag_f_1190 then
          let u_10809 = fail ()
          in
            bot()
         else () in
        f_without_checking_1199 prev_set_flag_f_1190 s_prev_f_x_1189 x_10376
in

let main r =
 let set_flag_f_1191m = false in
 let s_f_x_1188m = 0 in
 f_1036 set_flag_f_1191m s_f_x_1188m r set_flag_f_1191m s_f_x_1188m
 ()
in main 10