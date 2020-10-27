
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

   let compose_1030 x_DO_NOT_CARE_1433 x_DO_NOT_CARE_1434 f_EXPARAM_1133 x_DO_NOT_CARE_1431 x_DO_NOT_CARE_1432 f_1031 x_DO_NOT_CARE_1429 x_DO_NOT_CARE_1430 g_EXPARAM_1134 x_DO_NOT_CARE_1427 x_DO_NOT_CARE_1428 g_1032 set_flag_id_1234 s_id_x_1231 x_1033 =
     f_1031 set_flag_id_1234 s_id_x_1231
       (g_1032 set_flag_id_1234 s_id_x_1231 x_1033)

   let id_without_checking_1260 set_flag_id_1234 s_id_x_1231 x_1035 =
     let set_flag_id_1234 = true
     in
     let s_id_x_1231 = x_1035
     in
       x_1035

   let rec id_1034 prev_set_flag_id_1233 s_prev_id_x_1232 x_1035 =
     let u = if prev_set_flag_id_1233 then
              let u_6367 = fail ()
              in
                bot()
             else () in
            id_without_checking_1260 prev_set_flag_id_1233 s_prev_id_x_1232
              x_1035

   let succ_1036 set_flag_id_1234 s_id_x_1231 x_1037 = x_1037 + 1

   let rec toChurch_1038 x_DO_NOT_CARE_1425 x_DO_NOT_CARE_1426 n_1039 x_DO_NOT_CARE_1423 x_DO_NOT_CARE_1424 f_EXPARAM_1119 set_flag_id_1234 s_id_x_1231 f_1040 =
     if n_1039 = 0 then
       id_1034
     else
       compose_1030 set_flag_id_1234 s_id_x_1231
         ((c2_COEFFICIENT_1120 * f_EXPARAM_1119) +
          ((c3_COEFFICIENT_1121 * n_1039) + c4_COEFFICIENT_1122))
         set_flag_id_1234 s_id_x_1231 f_1040 set_flag_id_1234 s_id_x_1231
         ((c8_COEFFICIENT_1127 * f_EXPARAM_1119) +
          ((c9_COEFFICIENT_1128 * n_1039) + c10_COEFFICIENT_1129))
         set_flag_id_1234 s_id_x_1231
         (toChurch_1038 set_flag_id_1234 s_id_x_1231 (n_1039 - 1)
           set_flag_id_1234 s_id_x_1231
           ((c5_COEFFICIENT_1123 * f_EXPARAM_1119) +
            ((c6_COEFFICIENT_1124 * n_1039) + c7_COEFFICIENT_1125))
           set_flag_id_1234 s_id_x_1231 f_1040)

   let main (x_1043:int(*-:{v:Int | true}*)) =
     let set_flag_id_1234 = false in
     let s_id_x_1231 = 0 in
       if x_1043 >= 0 then
         let tos_1044 =
           toChurch_1038 set_flag_id_1234 s_id_x_1231 x_1043 set_flag_id_1234
             s_id_x_1231 ((c0_COEFFICIENT_1116 * x_1043) + c1_COEFFICIENT_1117)
             set_flag_id_1234 s_id_x_1231 succ_1036
         in
           ()
       else
         ()
