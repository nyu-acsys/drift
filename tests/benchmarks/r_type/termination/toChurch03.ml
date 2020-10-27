
let rec bot _ = bot ()
let fail _ = assert false

   let rec c10_COEFFICIENT_1129 = 0
   let rec c9_COEFFICIENT_1128 = 0
   let rec c8_COEFFICIENT_1127 = 0
   let rec c7_COEFFICIENT_1125 = 0
   let rec c6_COEFFICIENT_1124 = 0
   let rec c5_COEFFICIENT_1123 = 0
   let rec c4_COEFFICIENT_1122 = 0
   let rec c3_COEFFICIENT_1121 = 0
   let rec c2_COEFFICIENT_1120 = 0
   let rec c1_COEFFICIENT_1117 = 0
   let rec c0_COEFFICIENT_1116 = 0

   let compose_1030 x_DO_NOT_CARE_1462 x_DO_NOT_CARE_1463 x_DO_NOT_CARE_1464 f_EXPARAM_1133 x_DO_NOT_CARE_1459 x_DO_NOT_CARE_1460 x_DO_NOT_CARE_1461 f_1031 x_DO_NOT_CARE_1456 x_DO_NOT_CARE_1457 x_DO_NOT_CARE_1458 g_EXPARAM_1134 x_DO_NOT_CARE_1453 x_DO_NOT_CARE_1454 x_DO_NOT_CARE_1455 g_1032 set_flag_toChurch_1362 s_toChurch_n_1355 s_toChurch_f_EXPARAM_1356 x_1033 =
     f_1031 set_flag_toChurch_1362 s_toChurch_n_1355 s_toChurch_f_EXPARAM_1356
       (g_1032 set_flag_toChurch_1362 s_toChurch_n_1355
         s_toChurch_f_EXPARAM_1356 x_1033)

   let id_1034 set_flag_toChurch_1362 s_toChurch_n_1355 s_toChurch_f_EXPARAM_1356 x_1035 =
     x_1035

   let succ_1036 set_flag_toChurch_1362 s_toChurch_n_1355 s_toChurch_f_EXPARAM_1356 x_1037 =
     x_1037 + 1

   let rec toChurch_without_checking_1376 x_DO_NOT_CARE_1450 x_DO_NOT_CARE_1451 x_DO_NOT_CARE_1452 n_1039 x_DO_NOT_CARE_1447 x_DO_NOT_CARE_1448 x_DO_NOT_CARE_1449 f_EXPARAM_1119 set_flag_toChurch_1362 s_toChurch_n_1355 s_toChurch_f_EXPARAM_1356 f_1040 =
     let set_flag_toChurch_1362 = true
     in
     let s_toChurch_f_EXPARAM_1356 = f_EXPARAM_1119
     in
     let s_toChurch_n_1355 = n_1039
     in
       if n_1039 = 0 then
         id_1034
       else
         compose_1030 set_flag_toChurch_1362 s_toChurch_n_1355
           s_toChurch_f_EXPARAM_1356
           ((c2_COEFFICIENT_1120 * f_EXPARAM_1119) +
            ((c3_COEFFICIENT_1121 * n_1039) + c4_COEFFICIENT_1122))
           set_flag_toChurch_1362 s_toChurch_n_1355 s_toChurch_f_EXPARAM_1356
           f_1040 set_flag_toChurch_1362 s_toChurch_n_1355
           s_toChurch_f_EXPARAM_1356
           ((c8_COEFFICIENT_1127 * f_EXPARAM_1119) +
            ((c9_COEFFICIENT_1128 * n_1039) + c10_COEFFICIENT_1129))
           set_flag_toChurch_1362 s_toChurch_n_1355 s_toChurch_f_EXPARAM_1356
           (toChurch_without_checking_1376 set_flag_toChurch_1362
             s_toChurch_n_1355 s_toChurch_f_EXPARAM_1356 (n_1039 - 1)
             set_flag_toChurch_1362 s_toChurch_n_1355 s_toChurch_f_EXPARAM_1356
             ((c5_COEFFICIENT_1123 * f_EXPARAM_1119) +
              ((c6_COEFFICIENT_1124 * n_1039) + c7_COEFFICIENT_1125))
             set_flag_toChurch_1362 s_toChurch_n_1355 s_toChurch_f_EXPARAM_1356
             f_1040)

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

   let main (x_1043:int(*-:{v:Int | true}*)) =
     let set_flag_toChurch_1362 = false in
     let s_toChurch_n_1355 = 0 in
     let s_toChurch_f_EXPARAM_1356 = 0 in
       if x_1043 >= 0 then
         let tos_1044 =
           toChurch_1038 set_flag_toChurch_1362 s_toChurch_n_1355
             s_toChurch_f_EXPARAM_1356 x_1043 set_flag_toChurch_1362
             s_toChurch_n_1355 s_toChurch_f_EXPARAM_1356
             ((c0_COEFFICIENT_1116 * x_1043) + c1_COEFFICIENT_1117)
             set_flag_toChurch_1362 s_toChurch_n_1355 s_toChurch_f_EXPARAM_1356
             succ_1036
         in
           ()
       else
         ()
