
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

let id_1030 set_flag_f_12600 s_f_x_EXPARAM_12490 s_f_y_EXPARAM_12510 s_f_z_12530 x_1031 =
 x_1031
in

let rec omega_1032 set_flag_f_12608 s_f_x_EXPARAM_12498 s_f_y_EXPARAM_12518 s_f_z_12538 x_1033 =
 omega_1032 set_flag_f_12608 s_f_x_EXPARAM_12498 s_f_y_EXPARAM_12518
   s_f_z_12538 x_1033
in

let rec f_without_checking_1285 x_DO_NOT_CARE_1419 x_DO_NOT_CARE_1420 x_DO_NOT_CARE_1421 x_DO_NOT_CARE_1422 x_EXPARAM_11334 x_DO_NOT_CARE_1415 x_DO_NOT_CARE_1416 x_DO_NOT_CARE_1417 x_DO_NOT_CARE_1418 x_1035 x_DO_NOT_CARE_1411 x_DO_NOT_CARE_1412 x_DO_NOT_CARE_1413 x_DO_NOT_CARE_1414 y_EXPARAM_11344 x_DO_NOT_CARE_1407 x_DO_NOT_CARE_1408 x_DO_NOT_CARE_1409 x_DO_NOT_CARE_1410 y_1036 set_flag_f_12603 s_f_x_EXPARAM_12493 s_f_y_EXPARAM_12513 s_f_z_1253 z_10377 =
 let set_flag_f_1260_r = true
 in
 let s_f_z_1253_r = z_10377
 in
 let s_f_y_EXPARAM_1251_r = y_EXPARAM_11344
 in
 let s_f_x_EXPARAM_1249_r = x_EXPARAM_11334
 in
   y_1036 set_flag_f_1260_r s_f_x_EXPARAM_1249_r s_f_y_EXPARAM_1251_r s_f_z_1253_r
     z_10377
in

let rec f_1034 x_DO_NOT_CARE_1299 x_DO_NOT_CARE_1300 x_DO_NOT_CARE_1301 x_DO_NOT_CARE_1302 x_EXPARAM_1133 x_DO_NOT_CARE_1295 x_DO_NOT_CARE_1296 x_DO_NOT_CARE_1297 x_DO_NOT_CARE_1298 x_1035 x_DO_NOT_CARE_1291 x_DO_NOT_CARE_1292 x_DO_NOT_CARE_1293 x_DO_NOT_CARE_1294 y_EXPARAM_1134 x_DO_NOT_CARE_1287 x_DO_NOT_CARE_1288 x_DO_NOT_CARE_1289 x_DO_NOT_CARE_1290 y_1036 prev_set_flag_f_1259 s_prev_f_x_EXPARAM_1254 s_prev_f_y_EXPARAM_1256 s_prev_f_z_1258 z_1037 =
 let u = if prev_set_flag_f_1259 then
          let u_7669 = fail ()
          in
            bot()
         else () in
        f_without_checking_1285 x_DO_NOT_CARE_1299 x_DO_NOT_CARE_1300
          x_DO_NOT_CARE_1301 x_DO_NOT_CARE_1302 x_EXPARAM_1133
          x_DO_NOT_CARE_1295 x_DO_NOT_CARE_1296 x_DO_NOT_CARE_1297
          x_DO_NOT_CARE_1298 x_1035 x_DO_NOT_CARE_1291 x_DO_NOT_CARE_1292
          x_DO_NOT_CARE_1293 x_DO_NOT_CARE_1294 y_EXPARAM_1134
          x_DO_NOT_CARE_1287 x_DO_NOT_CARE_1288 x_DO_NOT_CARE_1289
          x_DO_NOT_CARE_1290 y_1036 prev_set_flag_f_1259
          s_prev_f_x_EXPARAM_1254 s_prev_f_y_EXPARAM_1256 s_prev_f_z_1258
          z_1037
in

let rec app_1038 x_DO_NOT_CARE_1403 x_DO_NOT_CARE_1404 x_DO_NOT_CARE_1405 x_DO_NOT_CARE_1406 h_EXPARAM_1131 x_DO_NOT_CARE_1399 x_DO_NOT_CARE_1400 x_DO_NOT_CARE_1401 x_DO_NOT_CARE_1402 h_1039 set_flag_f_1260 s_f_x_EXPARAM_1249 s_f_y_EXPARAM_1251 s_f_z_1253 x_1040 =
 h_1039 set_flag_f_1260 s_f_x_EXPARAM_1249 s_f_y_EXPARAM_1251 s_f_z_1253
   x_1040
in

let main =
 f_1034 false 0 0 0 c5_COEFFICIENT_1125 false 0 0 0
   (app_1038 false 0 0 0 c4_COEFFICIENT_1123 false 0 0 0
     (f_without_checking_1285 false 0 0 0 c1_COEFFICIENT_1117 false 0 0 0
       (app_1038 false 0 0 0 c0_COEFFICIENT_1115 false 0 0 0 id_1030) false
       0 0 0 c3_COEFFICIENT_1120 false 0 0 0
       (app_1038 false 0 0 0 c2_COEFFICIENT_1118 false 0 0 0 omega_1032)))
   false 0 0 0 c7_COEFFICIENT_1128 false 0 0 0
   (app_1038 false 0 0 0 c6_COEFFICIENT_1126 false 0 0 0 id_1030) false 0 0
   0 1
in assert(main = 1)
