
let main n_1038 x_1039 =
let rec bot bx = bot () in
let fail fx = assert (false) in

let c1_COEFFICIENT_1086 = 0 in
let c0_COEFFICIENT_1085 = 0 in

let succ_1030 set_flag_g_11340 s_g_r_EXPARAM_11270 s_g_a_11290 n_10310 =
 n_10310 + 1
in

let rec g_without_checking_1152 x_DO_NOT_CARE_1208 x_DO_NOT_CARE_1209 x_DO_NOT_CARE_1210 r_EXPARAM_10882 x_DO_NOT_CARE_1205 x_DO_NOT_CARE_1206 x_DO_NOT_CARE_1207 r_1033 set_flag_g_1134 s_g_r_EXPARAM_1127 s_g_a_1129 a_10342 =
 let set_flag_g_1134_r = true
 in
 let s_g_a_1129_r = a_10342
 in
 let s_g_r_EXPARAM_1127_r = r_EXPARAM_10882
 in
   r_1033 set_flag_g_1134_r s_g_r_EXPARAM_1127_r s_g_a_1129_r
     (r_1033 set_flag_g_1134_r s_g_r_EXPARAM_1127_r s_g_a_1129_r a_10342)
in

let rec g_1032 x_DO_NOT_CARE_1157 x_DO_NOT_CARE_1158 x_DO_NOT_CARE_1159 r_EXPARAM_1088 x_DO_NOT_CARE_1154 x_DO_NOT_CARE_1155 x_DO_NOT_CARE_1156 r_10332 prev_set_flag_g_1133 s_prev_g_r_EXPARAM_1130 s_prev_g_a_1132 a_1034 =
 let u =  if prev_set_flag_g_1133 then
          (if ((2 * 1) + (-4 * s_prev_g_r_EXPARAM_1130)) +
             (2 * s_prev_g_a_1132) >
             ((0 * 1) + (-4 * r_EXPARAM_1088)) + (0 * a_1034) &&
             ((0 * 1) + (-4 * r_EXPARAM_1088)) + (0 * a_1034) >= 0 then
            ()
          else
            let u_5939 = fail ()
            in
              bot())
          else () in
        g_without_checking_1152 x_DO_NOT_CARE_1157 x_DO_NOT_CARE_1158
          x_DO_NOT_CARE_1159 r_EXPARAM_1088 x_DO_NOT_CARE_1154
          x_DO_NOT_CARE_1155 x_DO_NOT_CARE_1156 r_10332 prev_set_flag_g_1133
          s_prev_g_r_EXPARAM_1130 s_prev_g_a_1132 a_1034
in

let rec f_1035 set_flag_g_1134 s_g_r_EXPARAM_1127 s_g_a_1129 n_1036 =
   if n_1036 = 0 then
     succ_1030
   else
     g_1032 set_flag_g_1134 s_g_r_EXPARAM_1127 s_g_a_1129
       ((c0_COEFFICIENT_1085 * n_1036) + c1_COEFFICIENT_1086) set_flag_g_1134
       s_g_r_EXPARAM_1127 s_g_a_1129
       (f_1035 set_flag_g_1134 s_g_r_EXPARAM_1127 s_g_a_1129 (n_1036 - 1))
in

 let x_DO_NOT_CARE_1202 = false in
 let x_DO_NOT_CARE_1203 = 0 in
 let x_DO_NOT_CARE_1204 = 0 in
 let set_flag_g_1134m = false in
 let s_g_r_EXPARAM_1127m = 0 in
 let s_g_a_1129m = 0 in
 if n_1038 >= 0 && x_1039 >= 0 then
   f_1035 set_flag_g_1134m s_g_r_EXPARAM_1127m s_g_a_1129m n_1038
     set_flag_g_1134m s_g_r_EXPARAM_1127m s_g_a_1129m x_1039
 else
   0
in assert (main 3 0 > 0)