
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

   let f1_1030 x_DO_NOT_CARE_1806 u_1031 x_DO_NOT_CARE_1805 c_EXPARAM_1293 x_DO_NOT_CARE_1804 c_1032 set_flag_f4_1592 d_1033 =
     d_1033

   let f2_1034 x_DO_NOT_CARE_1803 u_1035 x_DO_NOT_CARE_1802 a_EXPARAM_1279 x_DO_NOT_CARE_1801 a_1036 x_DO_NOT_CARE_1800 b_EXPARAM_1282 set_flag_f4_1592 b_1037 =
     a_1036 set_flag_f4_1592
       ((c7_COEFFICIENT_1284 * b_EXPARAM_1282) +
        ((c8_COEFFICIENT_1285 * a_EXPARAM_1279) + c9_COEFFICIENT_1286))
       set_flag_f4_1592 (f1_1030 set_flag_f4_1592 u_1035)

   let f3_1038 x_DO_NOT_CARE_1799 u_1039 x_DO_NOT_CARE_1798 a_EXPARAM_1261 set_flag_f4_1592 a_1040 =
     a_1040 set_flag_f4_1592
       ((c5_COEFFICIENT_1272 * a_EXPARAM_1261) + c6_COEFFICIENT_1273)
       set_flag_f4_1592
       (f2_1034 set_flag_f4_1592 u_1039 set_flag_f4_1592
         ((c3_COEFFICIENT_1266 * a_EXPARAM_1261) + c4_COEFFICIENT_1267)
         set_flag_f4_1592 a_1040)

   let f4_without_checking_1613 x_DO_NOT_CARE_1797 u_1042 set_flag_f4_1592 v_1043 =
     let set_flag_f4_1592 = true
     in
       v_1043

   let rec f4_1041 x_DO_NOT_CARE_1615 u_1042 prev_set_flag_f4_1591 v_1043 =
     let u =if prev_set_flag_f4_1591 then
              let u_19355 = fail ()
              in
                bot()
            else () in
            f4_without_checking_1613 x_DO_NOT_CARE_1615 u_1042
              prev_set_flag_f4_1591 v_1043

   let f5_1044 x_DO_NOT_CARE_1796 u_1045 x_DO_NOT_CARE_1795 e_EXPARAM_1254 set_flag_f4_1592 e_1046 =
     e_1046 set_flag_f4_1592
       ((c1_COEFFICIENT_1256 * e_EXPARAM_1254) + c2_COEFFICIENT_1257)
       set_flag_f4_1592 (f4_1041 set_flag_f4_1592 u_1045)

   let main_1047 set_flag_f4_1592 u_1048 =
     let zz_1032_1049 =
       f3_1038 set_flag_f4_1592 u_1048 set_flag_f4_1592 c0_COEFFICIENT_1250
         set_flag_f4_1592 (f5_1044 set_flag_f4_1592 u_1048)
     in
       ()

   let main =
     main_1047 false ()
