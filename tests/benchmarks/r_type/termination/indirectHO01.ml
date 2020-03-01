
let rec bot bx = bot () in
let fail fx = assert (false) in

let c1_COEFFICIENT_1088 = 0 in
let c0_COEFFICIENT_1087 = 0 in

let id_1030 set_flag_app_11380 s_app_h_EXPARAM_11310 x_1031 = x_1031 in

let app_without_checking_1158 x_DO_NOT_CARE_1213 x_DO_NOT_CARE_1214 h_EXPARAM_10908 x_DO_NOT_CARE_1211 x_DO_NOT_CARE_1212 h_10330 set_flag_app_11380 s_app_h_EXPARAM_11310 v_10340 =
 let set_flag_app_1138_r = true
 in
 let s_app_h_EXPARAM_1131_r = h_EXPARAM_10908
 in
   h_10330 set_flag_app_1138_r s_app_h_EXPARAM_1131_r () set_flag_app_1138_r
     s_app_h_EXPARAM_1131_r v_10340
in

let rec app_1032 x_DO_NOT_CARE_1162 x_DO_NOT_CARE_1163 h_EXPARAM_1090 x_DO_NOT_CARE_1160 x_DO_NOT_CARE_1161 h_1033 prev_set_flag_app_1137 s_prev_app_h_EXPARAM_1134 v_1034 =
 let u = if prev_set_flag_app_1137 then
          if (7 * 1) + (1 * s_prev_app_h_EXPARAM_1134) >
             (7 * 1) + (1 * h_EXPARAM_1090) &&
             (7 * 1) + (1 * h_EXPARAM_1090) >= 0 then
            ()
          else
            let u_4585 = fail ()
            in
              bot()
         else () in
        app_without_checking_1158 x_DO_NOT_CARE_1162 x_DO_NOT_CARE_1163
          h_EXPARAM_1090 x_DO_NOT_CARE_1160 x_DO_NOT_CARE_1161 h_1033
          prev_set_flag_app_1137 s_prev_app_h_EXPARAM_1134 v_1034
in

let rec f_1035 x_DO_NOT_CARE_1209 x_DO_NOT_CARE_1210 n_1036 set_flag_app_1138 s_app_h_EXPARAM_1131 u_1037 =
 if n_1036 > 0 then
   app_1032 set_flag_app_1138 s_app_h_EXPARAM_1131
     ((c0_COEFFICIENT_1087 * n_1036) + c1_COEFFICIENT_1088)
     set_flag_app_1138 s_app_h_EXPARAM_1131
     (f_1035 set_flag_app_1138 s_app_h_EXPARAM_1131 (n_1036 - 1))
 else
   id_1030
in

let main r =
 let set_flag_app_1138m = false in
 let s_app_h_EXPARAM_1131m = 0 in
 f_1035 set_flag_app_1138m s_app_h_EXPARAM_1131m 0
   set_flag_app_1138m s_app_h_EXPARAM_1131m () set_flag_app_1138m
   s_app_h_EXPARAM_1131m ()
in main 46