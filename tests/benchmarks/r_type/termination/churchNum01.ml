
let rec bot _ = bot ()
let fail _ = assert false

   let rec c12_COEFFICIENT_1238 = 0
   let rec c11_COEFFICIENT_1237 = 0
   let rec c10_COEFFICIENT_1236 = 0
   let rec c9_COEFFICIENT_1235 = 0
   let rec c8_COEFFICIENT_1221 = 0
   let rec c7_COEFFICIENT_1220 = 0
   let rec c6_COEFFICIENT_1219 = 0
   let rec c5_COEFFICIENT_1215 = 0
   let rec c4_COEFFICIENT_1214 = 0
   let rec c3_COEFFICIENT_1213 = 0
   let rec c2_COEFFICIENT_1196 = 0
   let rec c1_COEFFICIENT_1195 = 0
   let rec c0_COEFFICIENT_1193 = 0

   let rec succ_1030 x_DO_NOT_CARE_1596 x_DO_NOT_CARE_1597 m_EXPARAM_1232 x_DO_NOT_CARE_1594 x_DO_NOT_CARE_1595 m_1031 x_DO_NOT_CARE_1592 x_DO_NOT_CARE_1593 s_EXPARAM_1234 x_DO_NOT_CARE_1590 x_DO_NOT_CARE_1591 s_1032 set_flag_id_1346 s_id_x_1343 z_1033 =
     m_1031 set_flag_id_1346 s_id_x_1343
       ((c9_COEFFICIENT_1235 * z_1033) +
        ((c10_COEFFICIENT_1236 * s_EXPARAM_1234) +
         ((c11_COEFFICIENT_1237 * m_EXPARAM_1232) + c12_COEFFICIENT_1238)))
       set_flag_id_1346 s_id_x_1343 s_1032 set_flag_id_1346 s_id_x_1343
       (s_1032 set_flag_id_1346 s_id_x_1343 z_1033)

   let id_without_checking_1376 set_flag_id_1346 s_id_x_1343 x_1035 =
     let set_flag_id_1346 = true
     in
     let s_id_x_1343 = x_1035
     in
       x_1035

   let rec id_1034 prev_set_flag_id_1345 s_prev_id_x_1344 x_1035 =
     let u =if prev_set_flag_id_1345 then
              let u_26559 = fail ()
              in
                bot()
            else () in
            id_without_checking_1376 prev_set_flag_id_1345 s_prev_id_x_1344
              x_1035

   let rec two_1036 x_DO_NOT_CARE_1588 x_DO_NOT_CARE_1589 f_EXPARAM_1206 x_DO_NOT_CARE_1586 x_DO_NOT_CARE_1587 f_1037 x_DO_NOT_CARE_1584 x_DO_NOT_CARE_1585 z_EXPARAM_1210 set_flag_id_1346 s_id_x_1343 z_1038 =
     f_1037 set_flag_id_1346 s_id_x_1343
       ((c6_COEFFICIENT_1219 * z_EXPARAM_1210) +
        ((c7_COEFFICIENT_1220 * f_EXPARAM_1206) + c8_COEFFICIENT_1221))
       set_flag_id_1346 s_id_x_1343
       (f_1037 set_flag_id_1346 s_id_x_1343
         ((c3_COEFFICIENT_1213 * z_EXPARAM_1210) +
          ((c4_COEFFICIENT_1214 * f_EXPARAM_1206) + c5_COEFFICIENT_1215))
         set_flag_id_1346 s_id_x_1343 z_1038)

   let rec zero_1039 x_DO_NOT_CARE_1582 x_DO_NOT_CARE_1583 f_EXPARAM_1204 x_DO_NOT_CARE_1580 x_DO_NOT_CARE_1581 f_1040 set_flag_id_1346 s_id_x_1343 z_1041 =
     z_1041

   let main_1042 set_flag_id_1346 s_id_x_1343 u_1043 =
     two_1036 set_flag_id_1346 s_id_x_1343 c0_COEFFICIENT_1193 set_flag_id_1346
       s_id_x_1343 succ_1030 set_flag_id_1346 s_id_x_1343 c1_COEFFICIENT_1195
       set_flag_id_1346 s_id_x_1343 zero_1039 set_flag_id_1346 s_id_x_1343
       c2_COEFFICIENT_1196 set_flag_id_1346 s_id_x_1343 id_1034
       set_flag_id_1346 s_id_x_1343 0

   let main = main_1042 false 0 ()
