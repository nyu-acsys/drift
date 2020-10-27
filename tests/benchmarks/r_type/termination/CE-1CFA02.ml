
let rec bot _ = bot ()
let fail _ = assert false

   let rec c7_COEFFICIENT_1128 = 0
   let rec c6_COEFFICIENT_1126 = 0
   let rec c5_COEFFICIENT_1125 = 0
   let rec c4_COEFFICIENT_1123 = 0
   let rec c3_COEFFICIENT_1120 = 0
   let rec c2_COEFFICIENT_1118 = 0
   let rec c1_COEFFICIENT_1117 = 0
   let rec c0_COEFFICIENT_1115 = 0

   let id_1030 set_flag_omega_1196 s_omega_x_1193 x_1031 = x_1031

   let rec omega_without_checking_1227 set_flag_omega_1196 s_omega_x_1193 x_1033 =
     let set_flag_omega_1196 = true
     in
     let s_omega_x_1193 = x_1033
     in
       omega_without_checking_1227 set_flag_omega_1196 s_omega_x_1193 x_1033

   let rec omega_1032 prev_set_flag_omega_1195 s_prev_omega_x_1194 x_1033 =
     let u =if prev_set_flag_omega_1195 then
              let u_4056 = fail ()
              in
                bot()
            else () in
            omega_without_checking_1227 prev_set_flag_omega_1195
              s_prev_omega_x_1194 x_1033

   let f_1034 x_DO_NOT_CARE_1397 x_DO_NOT_CARE_1398 x_EXPARAM_1133 x_DO_NOT_CARE_1395 x_DO_NOT_CARE_1396 x_1035 x_DO_NOT_CARE_1393 x_DO_NOT_CARE_1394 y_EXPARAM_1134 x_DO_NOT_CARE_1391 x_DO_NOT_CARE_1392 y_1036 set_flag_omega_1196 s_omega_x_1193 z_1037 =
     y_1036 set_flag_omega_1196 s_omega_x_1193 z_1037

   let rec app_1038 x_DO_NOT_CARE_1389 x_DO_NOT_CARE_1390 h_EXPARAM_1131 x_DO_NOT_CARE_1387 x_DO_NOT_CARE_1388 h_1039 set_flag_omega_1196 s_omega_x_1193 x_1040 =
     h_1039 set_flag_omega_1196 s_omega_x_1193 x_1040

   let main =
     f_1034 false 0 c5_COEFFICIENT_1125 false 0
       (app_1038 false 0 c4_COEFFICIENT_1123 false 0
         (f_1034 false 0 c1_COEFFICIENT_1117 false 0
           (app_1038 false 0 c0_COEFFICIENT_1115 false 0 id_1030) false 0
           c3_COEFFICIENT_1120 false 0
           (app_1038 false 0 c2_COEFFICIENT_1118 false 0 omega_1032))) false 0
       c7_COEFFICIENT_1128 false 0
       (app_1038 false 0 c6_COEFFICIENT_1126 false 0 id_1030) false 0 1
