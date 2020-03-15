(*
Input data error
*)
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

let rec compose_without_checking_1188 x_DO_NOT_CARE_1419 x_DO_NOT_CARE_1420 x_DO_NOT_CARE_1421 x_DO_NOT_CARE_1422 f_EXPARAM_11338 x_DO_NOT_CARE_1415 x_DO_NOT_CARE_1416 x_DO_NOT_CARE_1417 x_DO_NOT_CARE_1418 f_10318 x_DO_NOT_CARE_1411 x_DO_NOT_CARE_1412 x_DO_NOT_CARE_1413 x_DO_NOT_CARE_1414 g_EXPARAM_11348 x_DO_NOT_CARE_1407 x_DO_NOT_CARE_1408 x_DO_NOT_CARE_1409 x_DO_NOT_CARE_1410 g_10328 set_flag_compose_1148r s_compose_f_EXPARAM_11378 s_compose_g_EXPARAM_11398 s_compose_x_11418 x_10338 =
 let set_flag_compose_1148_r = true
 in
 let s_compose_x_1141_r = x_10338
 in
 let s_compose_g_EXPARAM_1139_r = g_EXPARAM_11348
 in
 let s_compose_f_EXPARAM_1137_r = f_EXPARAM_11338
 in
   f_10318 set_flag_compose_1148_r s_compose_f_EXPARAM_1137_r
     s_compose_g_EXPARAM_1139_r s_compose_x_1141_r
     (g_10328 set_flag_compose_1148_r s_compose_f_EXPARAM_1137_r
       s_compose_g_EXPARAM_1139_r s_compose_x_1141_r x_10338)
in

let rec compose_1030 x_DO_NOT_CARE_1202 x_DO_NOT_CARE_1203 x_DO_NOT_CARE_1204 x_DO_NOT_CARE_1205 f_EXPARAM_1133 x_DO_NOT_CARE_1198 x_DO_NOT_CARE_1199 x_DO_NOT_CARE_1200 x_DO_NOT_CARE_1201 f_1031 x_DO_NOT_CARE_1194 x_DO_NOT_CARE_1195 x_DO_NOT_CARE_1196 x_DO_NOT_CARE_1197 g_EXPARAM_1134 x_DO_NOT_CARE_1190 x_DO_NOT_CARE_1191 x_DO_NOT_CARE_1192 x_DO_NOT_CARE_1193 g_1032 prev_set_flag_compose_1147 s_prev_compose_f_EXPARAM_1142 s_prev_compose_g_EXPARAM_1144 s_prev_compose_x_1146 x_1033 =
 let u =if prev_set_flag_compose_1147 then
          let u_2450 = fail ()
          in
            bot()
        else () in
        compose_without_checking_1188 x_DO_NOT_CARE_1202 x_DO_NOT_CARE_1203
          x_DO_NOT_CARE_1204 x_DO_NOT_CARE_1205 f_EXPARAM_1133
          x_DO_NOT_CARE_1198 x_DO_NOT_CARE_1199 x_DO_NOT_CARE_1200
          x_DO_NOT_CARE_1201 f_1031 x_DO_NOT_CARE_1194 x_DO_NOT_CARE_1195
          x_DO_NOT_CARE_1196 x_DO_NOT_CARE_1197 g_EXPARAM_1134
          x_DO_NOT_CARE_1190 x_DO_NOT_CARE_1191 x_DO_NOT_CARE_1192
          x_DO_NOT_CARE_1193 g_1032 prev_set_flag_compose_1147
          s_prev_compose_f_EXPARAM_1142 s_prev_compose_g_EXPARAM_1144
          s_prev_compose_x_1146 x_1033
in

let id_1034 set_flag_compose_11484 s_compose_f_EXPARAM_11374 s_compose_g_EXPARAM_11394 s_compose_x_11414 x_1035 =
 x_1035
in

let succ_1036 set_flag_compose_11486 s_compose_f_EXPARAM_11376 s_compose_g_EXPARAM_11396 s_compose_x_11416 x_1037 =
 x_1037 + 1
in

let rec toChurch_1038 x_DO_NOT_CARE_1403 x_DO_NOT_CARE_1404 x_DO_NOT_CARE_1405 x_DO_NOT_CARE_1406 n_1039 x_DO_NOT_CARE_1399 x_DO_NOT_CARE_1400 x_DO_NOT_CARE_1401 x_DO_NOT_CARE_1402 f_EXPARAM_1119 set_flag_compose_1148 s_compose_f_EXPARAM_1137 s_compose_g_EXPARAM_1139 s_compose_x_1141 f_1040 =
 if n_1039 = 0 then
   id_1034
 else
   compose_1030 set_flag_compose_1148 s_compose_f_EXPARAM_1137
     s_compose_g_EXPARAM_1139 s_compose_x_1141
     ((c2_COEFFICIENT_1120 * f_EXPARAM_1119) +
      ((c3_COEFFICIENT_1121 * n_1039) + c4_COEFFICIENT_1122))
     set_flag_compose_1148 s_compose_f_EXPARAM_1137
     s_compose_g_EXPARAM_1139 s_compose_x_1141 f_1040 set_flag_compose_1148
     s_compose_f_EXPARAM_1137 s_compose_g_EXPARAM_1139 s_compose_x_1141
     ((c8_COEFFICIENT_1127 * f_EXPARAM_1119) +
      ((c9_COEFFICIENT_1128 * n_1039) + c10_COEFFICIENT_1129))
     set_flag_compose_1148 s_compose_f_EXPARAM_1137
     s_compose_g_EXPARAM_1139 s_compose_x_1141
     (id_1034)
in

  let set_flag_compose_1148m = false in
  let s_compose_f_EXPARAM_1137m = 0 in
  let s_compose_g_EXPARAM_1139m = 0 in
  let s_compose_x_1141m = 0 in
   let tos_1044 =
     toChurch_1038 set_flag_compose_1148m s_compose_f_EXPARAM_1137m
       s_compose_g_EXPARAM_1139m s_compose_x_1141m x_1043
       set_flag_compose_1148m s_compose_f_EXPARAM_1137m
       s_compose_g_EXPARAM_1139m s_compose_x_1141m
       ((c0_COEFFICIENT_1116 * x_1043) + c1_COEFFICIENT_1117)
       set_flag_compose_1148m s_compose_f_EXPARAM_1137m
       s_compose_g_EXPARAM_1139m s_compose_x_1141m succ_1036
       set_flag_compose_1148m c1_COEFFICIENT_1117 set_flag_compose_1148m c1_COEFFICIENT_1117 x_1043
   in
   assert(tos_1044 > x_1043)
in main (4611686018427387903)