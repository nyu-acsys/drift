
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

   let compose_1030 x_DO_NOT_CARE_1445 x_DO_NOT_CARE_1446 f_EXPARAM_1133 x_DO_NOT_CARE_1443 x_DO_NOT_CARE_1444 f_1031 x_DO_NOT_CARE_1441 x_DO_NOT_CARE_1442 g_EXPARAM_1134 x_DO_NOT_CARE_1439 x_DO_NOT_CARE_1440 g_1032 set_flag_succ_1296 s_succ_x_1293 x_1033 =
     f_1031 set_flag_succ_1296 s_succ_x_1293
       (g_1032 set_flag_succ_1296 s_succ_x_1293 x_1033)

   let id_1034 set_flag_succ_1296 s_succ_x_1293 x_1035 = x_1035

   let succ_without_checking_1316 set_flag_succ_1296 s_succ_x_1293 x_1037 =
     let set_flag_succ_1296 = true
     in
     let s_succ_x_1293 = x_1037
     in
       x_1037 + 1

   let rec succ_1036 prev_set_flag_succ_1295 s_prev_succ_x_1294 x_1037 =
     let u = if prev_set_flag_succ_1295 then
              let u_8154 = fail ()
              in
                bot()
             else () in
            succ_without_checking_1316 prev_set_flag_succ_1295
              s_prev_succ_x_1294 x_1037

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

   let main (x_1043:int(*-:{v:Int | true}*)) =
     let set_flag_succ_1296 = false in
     let s_succ_x_1293 = 0 in
       if x_1043 >= 0 then
         let tos_1044 =
           toChurch_1038 set_flag_succ_1296 s_succ_x_1293 x_1043
             set_flag_succ_1296 s_succ_x_1293
             ((c0_COEFFICIENT_1116 * x_1043) + c1_COEFFICIENT_1117)
             set_flag_succ_1296 s_succ_x_1293 succ_1036
         in
           ()
       else
         ()
