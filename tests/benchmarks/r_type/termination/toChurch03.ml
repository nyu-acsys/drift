
let main x_1043 =
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

let compose_1030 x_DO_NOT_CARE_1462 x_DO_NOT_CARE_1463 x_DO_NOT_CARE_1464 f_EXPARAM_1133 x_DO_NOT_CARE_1459 x_DO_NOT_CARE_1460 x_DO_NOT_CARE_1461 f_1031 x_DO_NOT_CARE_1456 x_DO_NOT_CARE_1457 x_DO_NOT_CARE_1458 g_EXPARAM_1134 x_DO_NOT_CARE_1453 x_DO_NOT_CARE_1454 x_DO_NOT_CARE_1455 g_1032 set_flag_toChurch_13620 s_toChurch_n_13550 s_toChurch_f_EXPARAM_13560 x_1033 =
 f_1031 set_flag_toChurch_13620 s_toChurch_n_13550 s_toChurch_f_EXPARAM_13560
   (g_1032 set_flag_toChurch_13620 s_toChurch_n_13550
     s_toChurch_f_EXPARAM_13560 x_1033)
in

let id_1034 set_flag_toChurch_1362i s_toChurch_n_1355i s_toChurch_f_EXPARAM_1356i x_1035 =
 x_1035
in

let succ_1036 set_flag_toChurch_1362s s_toChurch_n_1355s s_toChurch_f_EXPARAM_1356s x_1037 =
 x_1037 + 1
in

let rec toChurch_without_checking_1376 x_DO_NOT_CARE_1450 x_DO_NOT_CARE_1451 x_DO_NOT_CARE_1452 n_1039 x_DO_NOT_CARE_1447 x_DO_NOT_CARE_1448 x_DO_NOT_CARE_1449 f_EXPARAM_1119 set_flag_toChurch_1362 s_toChurch_n_1355 s_toChurch_f_EXPARAM_1356 f_1040 =
 let set_flag_toChurch_1362_r = true
 in
 let s_toChurch_f_EXPARAM_1356_r = f_EXPARAM_1119
 in
 let s_toChurch_n_1355_r = n_1039
 in
   if n_1039 = 0 then
     id_1034
   else
     compose_1030 set_flag_toChurch_1362_r s_toChurch_n_1355_r
       s_toChurch_f_EXPARAM_1356_r
       ((c2_COEFFICIENT_1120 * f_EXPARAM_1119) +
        ((c3_COEFFICIENT_1121 * n_1039) + c4_COEFFICIENT_1122))
       set_flag_toChurch_1362_r s_toChurch_n_1355_r s_toChurch_f_EXPARAM_1356_r
       f_1040 set_flag_toChurch_1362_r s_toChurch_n_1355_r
       s_toChurch_f_EXPARAM_1356_r
       ((c8_COEFFICIENT_1127 * f_EXPARAM_1119) +
        ((c9_COEFFICIENT_1128 * n_1039) + c10_COEFFICIENT_1129))
       set_flag_toChurch_1362_r s_toChurch_n_1355_r s_toChurch_f_EXPARAM_1356_r
       (toChurch_without_checking_1376 set_flag_toChurch_1362_r
         s_toChurch_n_1355_r s_toChurch_f_EXPARAM_1356_r (n_1039 - 1)
         set_flag_toChurch_1362_r s_toChurch_n_1355_r s_toChurch_f_EXPARAM_1356_r
         ((c5_COEFFICIENT_1123 * f_EXPARAM_1119) +
          ((c6_COEFFICIENT_1124 * n_1039) + c7_COEFFICIENT_1125))
         set_flag_toChurch_1362_r s_toChurch_n_1355_r s_toChurch_f_EXPARAM_1356_r
         f_1040)
in

let rec toChurch_1038 x_DO_NOT_CARE_1381 x_DO_NOT_CARE_1382 x_DO_NOT_CARE_1383 n_1039 x_DO_NOT_CARE_1378 x_DO_NOT_CARE_1379 x_DO_NOT_CARE_1380 f_EXPARAM_1119 prev_set_flag_toChurch_1361 s_prev_toChurch_n_1358 s_prev_toChurch_f_EXPARAM_1359 f_1040 =
 let u = if prev_set_flag_toChurch_1361 then
          let u_10600 = fail ()
          in
            bot()
         else () in
        toChurch_without_checking_1376 x_DO_NOT_CARE_1381
          x_DO_NOT_CARE_1382 x_DO_NOT_CARE_1383 n_1039 x_DO_NOT_CARE_1378
          x_DO_NOT_CARE_1379 x_DO_NOT_CARE_1380 f_EXPARAM_1119
          prev_set_flag_toChurch_1361 s_prev_toChurch_n_1358
          s_prev_toChurch_f_EXPARAM_1359 f_1040
in

 let set_flag_toChurch_1362m = false in
 let s_toChurch_n_1355m = 0 in
 let s_toChurch_f_EXPARAM_1356m = 0 in
   if x_1043 >= 0 then
     let tos_1044 =
       toChurch_1038 set_flag_toChurch_1362m s_toChurch_n_1355m
         s_toChurch_f_EXPARAM_1356m x_1043 set_flag_toChurch_1362m
         s_toChurch_n_1355m s_toChurch_f_EXPARAM_1356m
         ((c0_COEFFICIENT_1116 * x_1043) + c1_COEFFICIENT_1117)
         set_flag_toChurch_1362m s_toChurch_n_1355m s_toChurch_f_EXPARAM_1356m
         succ_1036
     in
       ()
   else
     ()
in main 10
