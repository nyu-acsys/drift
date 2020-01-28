
let rec bot bx = bot () in
let fail fx = assert (false) in

let c10_COEFFICIENT_1129 = 0 in
let c9_COEFFICIENT_1128 = 0 in
let c8_COEFFICIENT_1127 = 0 in
let c7_COEFFICIENT_1125 = 0 in
let c6_COEFFICIENT_1124 = 0 in
let c5_COEFFICIENT_1123 = 0 in
let c4_COEFFICIENT_1122 = 0 in
let c3_COEFFICIENT_1121 = 0 in
let c2_COEFFICIENT_1120 = 0 in
let c1_COEFFICIENT_1117 = 0 in
let c0_COEFFICIENT_1116 = 0 in

let compose_1030 x_DO_NOT_CARE_1433 x_DO_NOT_CARE_1434 f_EXPARAM_1133 x_DO_NOT_CARE_1431 x_DO_NOT_CARE_1432 f_1031 x_DO_NOT_CARE_1429 x_DO_NOT_CARE_1430 g_EXPARAM_1134 x_DO_NOT_CARE_1427 x_DO_NOT_CARE_1428 g_1032 set_flag_id_12340 s_id_x_12310 x_1033 =
 f_1031 set_flag_id_12340 s_id_x_12310
   (g_1032 set_flag_id_12340 s_id_x_12310 x_1033)
in

let id_1034 set_flag_succ_12964 s_succ_x_12934 x_1035 = x_1035 in

let succ_without_checking_1316 set_flag_succ_12966 s_succ_x_12936 x_10376 =
 let set_flag_succ_1296_r = true
 in
 let s_succ_x_1293_r = x_10376
 in
   x_10376 + 1
in

let rec succ_1036 prev_set_flag_succ_1295 s_prev_succ_x_1294 x_1037 =
 let u = if prev_set_flag_succ_1295 then
          let u_8154 = fail ()
          in
            bot()
         else () in
        succ_without_checking_1316 prev_set_flag_succ_1295
          s_prev_succ_x_1294 x_1037
in

let rec toChurch_1038 x_DO_NOT_CARE_1437 x_DO_NOT_CARE_1438 n_1039 x_DO_NOT_CARE_1435 x_DO_NOT_CARE_1436 f_EXPARAM_1119 set_flag_succ_1296 s_succ_x_1293 f_1040 =
 if n_1039 = 0 then
   id_1034
 else
   compose_1030 set_flag_succ_1296 s_succ_x_1293
     ((c2_COEFFICIENT_1120 * f_EXPARAM_1119) +
      ((c3_COEFFICIENT_1121 * n_1039) + c4_COEFFICIENT_1122))
     set_flag_succ_1296 s_succ_x_1293 f_1040 set_flag_succ_1296
     s_succ_x_1293
     ((c8_COEFFICIENT_1127 * f_EXPARAM_1119) +
      ((c9_COEFFICIENT_1128 * n_1039) + c10_COEFFICIENT_1129))
     set_flag_succ_1296 s_succ_x_1293
     (toChurch_1038 set_flag_succ_1296 s_succ_x_1293 (n_1039 - 1)
       set_flag_succ_1296 s_succ_x_1293
       ((c5_COEFFICIENT_1123 * f_EXPARAM_1119) +
        ((c6_COEFFICIENT_1124 * n_1039) + c7_COEFFICIENT_1125))
       set_flag_succ_1296 s_succ_x_1293 f_1040)
in

let main x_1043 =
 let set_flag_succ_1296m = false in
 let s_succ_x_1293m = 0 in
   if x_1043 >= 0 then
     let tos_1044 =
       toChurch_1038 set_flag_succ_1296m s_succ_x_1293m x_1043
         set_flag_succ_1296m s_succ_x_1293m
         ((c0_COEFFICIENT_1116 * x_1043) + c1_COEFFICIENT_1117)
         set_flag_succ_1296m s_succ_x_1293m succ_1036
     in
       ()
   else
     ()
in main 100
