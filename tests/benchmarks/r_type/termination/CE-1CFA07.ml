let main =
let rec bot bx = bot () in
let fail fx = assert (false) in

let c7_COEFFICIENT_1128 = 0 in
let c6_COEFFICIENT_1126 = 0 in
let c5_COEFFICIENT_1125 = 0 in
let c4_COEFFICIENT_1123 = 0 in
let c3_COEFFICIENT_1120 = 0 in
let c2_COEFFICIENT_1118 = 0 in
let c1_COEFFICIENT_1117 = 0 in
let c0_COEFFICIENT_1115 = 0 in

let id_1030 set_flag_app_13440 s_app_h_EXPARAM_13370 s_app_x_13390 x_1031 =
 x_1031
in

let rec omega_1032 set_flag_app_13442 s_app_h_EXPARAM_13372 s_app_x_13392 x_1033 =
 omega_1032 set_flag_app_13442 s_app_h_EXPARAM_13372 s_app_x_13392 x_1033
in

let f_1034 x_DO_NOT_CARE_1438 x_DO_NOT_CARE_1439 x_DO_NOT_CARE_1440 x_EXPARAM_1133 x_DO_NOT_CARE_1435 x_DO_NOT_CARE_1436 x_DO_NOT_CARE_1437 x_1035 x_DO_NOT_CARE_1432 x_DO_NOT_CARE_1433 x_DO_NOT_CARE_1434 y_EXPARAM_1134 x_DO_NOT_CARE_1429 x_DO_NOT_CARE_1430 x_DO_NOT_CARE_1431 y_1036 set_flag_app_1344 s_app_h_EXPARAM_1337 s_app_x_1339 z_1037 =
 y_1036 set_flag_app_1344 s_app_h_EXPARAM_1337 s_app_x_1339 z_1037
in

let app_without_checking_1355 x_DO_NOT_CARE_1426 x_DO_NOT_CARE_1427 x_DO_NOT_CARE_1428 h_EXPARAM_11317 x_DO_NOT_CARE_1423 x_DO_NOT_CARE_1424 x_DO_NOT_CARE_1425 h_10397 set_flag_app_13444 s_app_h_EXPARAM_13377 s_app_x_13397 x_10407 =
 let set_flag_app_1344_r = true
 in
 let s_app_x_1339_r = x_10407
 in
 let s_app_h_EXPARAM_1337_r = h_EXPARAM_11317
 in
   h_10397 set_flag_app_1344_r s_app_h_EXPARAM_1337_r s_app_x_1339_r x_10407
in

let rec app_1038 x_DO_NOT_CARE_1360 x_DO_NOT_CARE_1361 x_DO_NOT_CARE_1362 h_EXPARAM_1131 x_DO_NOT_CARE_1357 x_DO_NOT_CARE_1358 x_DO_NOT_CARE_1359 h_1039 prev_set_flag_app_1343 s_prev_app_h_EXPARAM_1340 s_prev_app_x_1342 x_1040 =
 let u = if prev_set_flag_app_1343 then
          let u_17381 = fail ()
          in
            bot()
         else () in
        app_without_checking_1355 x_DO_NOT_CARE_1360 x_DO_NOT_CARE_1361
          x_DO_NOT_CARE_1362 h_EXPARAM_1131 x_DO_NOT_CARE_1357
          x_DO_NOT_CARE_1358 x_DO_NOT_CARE_1359 h_1039
          prev_set_flag_app_1343 s_prev_app_h_EXPARAM_1340
          s_prev_app_x_1342 x_1040
in


 f_1034 false 0 0 c5_COEFFICIENT_1125 false 0 0
   (app_without_checking_1355 false 0 0 c4_COEFFICIENT_1123 false 0 0
     (f_1034 false 0 0 c1_COEFFICIENT_1117 false 0 0
       (app_without_checking_1355 false 0 0 c0_COEFFICIENT_1115 false 0 0
         id_1030) false 0 0 c3_COEFFICIENT_1120 false 0 0
       (app_1038 false 0 0 c2_COEFFICIENT_1118 false 0 0 omega_1032)))
   false 0 0 c7_COEFFICIENT_1128 false 0 0
   (app_without_checking_1355 false 0 0 c6_COEFFICIENT_1126 false 0 0
     id_1030) false 0 0 1
in assert(main = 1)
