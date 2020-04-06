

let rec bot bx = bot ()
let fail fx = assert (false)

let c1_COEFFICIENT_1088 = 0
let c0_COEFFICIENT_1087 = 0

let id_1030 set_flag_f_11870 s_f_n_11820 x_1031 = x_1031

let app_1032 x_DO_NOT_CARE_1219 x_DO_NOT_CARE_1220 h_EXPARAM_1090 x_DO_NOT_CARE_1217 x_DO_NOT_CARE_1218 h_1033 set_flag_f_1187 s_f_n_1182 v_1034 =
 h_1033 set_flag_f_1187 s_f_n_1182 () set_flag_f_1187 s_f_n_1182 v_1034

let rec f_1035 f_without x_DO_NOT_CARE_1199 x_DO_NOT_CARE_1200 n_1036 prev_set_flag_f_1186 s_prev_f_n_1184 u_1037 =
 let u  =if prev_set_flag_f_1186 then
          let u_9957 = fail ()
          in
            bot()
         else () in
        f_without x_DO_NOT_CARE_1199 x_DO_NOT_CARE_1200
          n_1036 prev_set_flag_f_1186 s_prev_f_n_1184 u_1037

let rec f_without_checking_1197 x_DO_NOT_CARE_1215 x_DO_NOT_CARE_1216 n_10367 set_flag_f_11877 s_f_n_11827 u_10377 =
 let set_flag_f_1187_r = true
 in
 let s_f_n_1182_r = n_10367
 in
   if n_10367 > 0 then
     app_1032 set_flag_f_1187_r s_f_n_1182_r
       ((c0_COEFFICIENT_1087 * n_10367) + c1_COEFFICIENT_1088)
       set_flag_f_1187_r s_f_n_1182_r
       (f_without_checking_1197 set_flag_f_1187_r s_f_n_1182_r (n_10367 - 1))
   else
     id_1030

let main r =
 let set_flag_f_1187m = false in
 let s_f_n_1182m = 0 in
 f_1035 f_without_checking_1197 set_flag_f_1187m s_f_n_1182m r set_flag_f_1187m s_f_n_1182m
   () set_flag_f_1187m s_f_n_1182m ()
