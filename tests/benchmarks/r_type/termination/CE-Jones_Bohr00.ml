
let rec bot _ = bot ()
let fail _ = assert false

   let rec c9_COEFFICIENT_1286 = 0
   let rec c8_COEFFICIENT_1285 = 0
   let rec c7_COEFFICIENT_1284 = 0
   let rec c6_COEFFICIENT_1273 = 0
   let rec c5_COEFFICIENT_1272 = 0
   let rec c4_COEFFICIENT_1267 = 0
   let rec c3_COEFFICIENT_1266 = 0
   let rec c2_COEFFICIENT_1257 = 0
   let rec c1_COEFFICIENT_1256 = 0
   let rec c0_COEFFICIENT_1250 = 0

   let f1_without_checking_1361 x_DO_NOT_CARE_1733 x_DO_NOT_CARE_1734 u_1031 x_DO_NOT_CARE_1731 x_DO_NOT_CARE_1732 c_EXPARAM_1293 x_DO_NOT_CARE_1729 x_DO_NOT_CARE_1730 c_1032 set_flag_f1_1304 s_f1_c_EXPARAM_1296 d_1033 =
     let set_flag_f1_1304 = true
     in
     let s_f1_c_EXPARAM_1296 = c_EXPARAM_1293
     in
       d_1033

   let rec f1_1030 x_DO_NOT_CARE_1367 x_DO_NOT_CARE_1368 u_1031 x_DO_NOT_CARE_1365 x_DO_NOT_CARE_1366 c_EXPARAM_1293 x_DO_NOT_CARE_1363 x_DO_NOT_CARE_1364 c_1032 prev_set_flag_f1_1303 s_prev_f1_c_EXPARAM_1300 d_1033 =
     let u = if prev_set_flag_f1_1303 then
              let u_2056 = fail ()
              in
                bot()
             else () in
            f1_without_checking_1361 x_DO_NOT_CARE_1367 x_DO_NOT_CARE_1368
              u_1031 x_DO_NOT_CARE_1365 x_DO_NOT_CARE_1366 c_EXPARAM_1293
              x_DO_NOT_CARE_1363 x_DO_NOT_CARE_1364 c_1032
              prev_set_flag_f1_1303 s_prev_f1_c_EXPARAM_1300 d_1033

   let f2_1034 x_DO_NOT_CARE_1727 x_DO_NOT_CARE_1728 u_1035 x_DO_NOT_CARE_1725 x_DO_NOT_CARE_1726 a_EXPARAM_1279 x_DO_NOT_CARE_1723 x_DO_NOT_CARE_1724 a_1036 x_DO_NOT_CARE_1721 x_DO_NOT_CARE_1722 b_EXPARAM_1282 set_flag_f1_1304 s_f1_c_EXPARAM_1296 b_1037 =
     a_1036 set_flag_f1_1304 s_f1_c_EXPARAM_1296
       ((c7_COEFFICIENT_1284 * b_EXPARAM_1282) +
        ((c8_COEFFICIENT_1285 * a_EXPARAM_1279) + c9_COEFFICIENT_1286))
       set_flag_f1_1304 s_f1_c_EXPARAM_1296
       (f1_1030 set_flag_f1_1304 s_f1_c_EXPARAM_1296 u_1035)

   let f3_1038 x_DO_NOT_CARE_1719 x_DO_NOT_CARE_1720 u_1039 x_DO_NOT_CARE_1717 x_DO_NOT_CARE_1718 a_EXPARAM_1261 set_flag_f1_1304 s_f1_c_EXPARAM_1296 a_1040 =
     a_1040 set_flag_f1_1304 s_f1_c_EXPARAM_1296
       ((c5_COEFFICIENT_1272 * a_EXPARAM_1261) + c6_COEFFICIENT_1273)
       set_flag_f1_1304 s_f1_c_EXPARAM_1296
       (f2_1034 set_flag_f1_1304 s_f1_c_EXPARAM_1296 u_1039 set_flag_f1_1304
         s_f1_c_EXPARAM_1296
         ((c3_COEFFICIENT_1266 * a_EXPARAM_1261) + c4_COEFFICIENT_1267)
         set_flag_f1_1304 s_f1_c_EXPARAM_1296 a_1040)

   let f4_1041 x_DO_NOT_CARE_1715 x_DO_NOT_CARE_1716 u_1042 set_flag_f1_1304 s_f1_c_EXPARAM_1296 v_1043 =
     v_1043

   let f5_1044 x_DO_NOT_CARE_1713 x_DO_NOT_CARE_1714 u_1045 x_DO_NOT_CARE_1711 x_DO_NOT_CARE_1712 e_EXPARAM_1254 set_flag_f1_1304 s_f1_c_EXPARAM_1296 e_1046 =
     e_1046 set_flag_f1_1304 s_f1_c_EXPARAM_1296
       ((c1_COEFFICIENT_1256 * e_EXPARAM_1254) + c2_COEFFICIENT_1257)
       set_flag_f1_1304 s_f1_c_EXPARAM_1296
       (f4_1041 set_flag_f1_1304 s_f1_c_EXPARAM_1296 u_1045)

   let main_1047 set_flag_f1_1304 s_f1_c_EXPARAM_1296 u_1048 =
     let zz_1032_1049 =
       f3_1038 set_flag_f1_1304 s_f1_c_EXPARAM_1296 u_1048 set_flag_f1_1304
         s_f1_c_EXPARAM_1296 c0_COEFFICIENT_1250 set_flag_f1_1304
         s_f1_c_EXPARAM_1296
         (f5_1044 set_flag_f1_1304 s_f1_c_EXPARAM_1296 u_1048)
     in
       ()

   let main =
     main_1047 false 0 ()
