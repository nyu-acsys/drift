let main =
let rec bot bx = bot () in
let fail fx = assert (false) in

let c3_COEFFICIENT_1089 = 0 in 
let c2_COEFFICIENT_1088 = 0 in
let c1_COEFFICIENT_1085 = 0 in
let c0_COEFFICIENT_1084 = 0 in

let id_1030 set_flag_omega_11410 s_omega_x_11380 x_1031 = x_1031 in

let rec omega_without_checking_1162 set_flag_omega_11412 s_omega_x_11382 x_10332 =
 let set_flag_omega_1141_r = true
 in
 let s_omega_x_1138_r = x_10332
 in
   omega_without_checking_1162 set_flag_omega_1141_r s_omega_x_1138_r x_10332
in

let rec omega_1032 prev_set_flag_omega_1140 s_prev_omega_x_1139 x_1033 =
 let u = if prev_set_flag_omega_1140 then
          let u_3120 = fail ()
          in
            bot()
         else () in
        omega_without_checking_1162 prev_set_flag_omega_1140
          s_prev_omega_x_1139 x_1033
in

let f_1034 x_DO_NOT_CARE_1245 x_DO_NOT_CARE_1246 x_EXPARAM_1092 x_DO_NOT_CARE_1243 x_DO_NOT_CARE_1244 x_1035 x_DO_NOT_CARE_1241 x_DO_NOT_CARE_1242 y_EXPARAM_1093 x_DO_NOT_CARE_1239 x_DO_NOT_CARE_1240 y_1036 set_flag_omega_1141 s_omega_x_1138 z_1037 =
 y_1036 set_flag_omega_1141 s_omega_x_1138 z_1037
in


 f_1034 false 0 c2_COEFFICIENT_1088 false 0
   (f_1034 false 0 c0_COEFFICIENT_1084 false 0 id_1030 false 0
     c1_COEFFICIENT_1085 false 0 omega_1032) false 0 c3_COEFFICIENT_1089
   false 0 id_1030 false 0 1
in assert(main = 1)