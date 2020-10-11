
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

   let f1_1030 x_DO_NOT_CARE_1829 x_DO_NOT_CARE_1830 u_1031 x_DO_NOT_CARE_1827 x_DO_NOT_CARE_1828 c_EXPARAM_1293 x_DO_NOT_CARE_1825 x_DO_NOT_CARE_1826 c_1032 set_flag_f5_1677 s_f5_e_EXPARAM_1671 d_1033 =
     d_1033

   let f2_1034 x_DO_NOT_CARE_1823 x_DO_NOT_CARE_1824 u_1035 x_DO_NOT_CARE_1821 x_DO_NOT_CARE_1822 a_EXPARAM_1279 x_DO_NOT_CARE_1819 x_DO_NOT_CARE_1820 a_1036 x_DO_NOT_CARE_1817 x_DO_NOT_CARE_1818 b_EXPARAM_1282 set_flag_f5_1677 s_f5_e_EXPARAM_1671 b_1037 =
     a_1036 set_flag_f5_1677 s_f5_e_EXPARAM_1671
       ((c7_COEFFICIENT_1284 * b_EXPARAM_1282) +
        ((c8_COEFFICIENT_1285 * a_EXPARAM_1279) + c9_COEFFICIENT_1286))
       set_flag_f5_1677 s_f5_e_EXPARAM_1671
       (f1_1030 set_flag_f5_1677 s_f5_e_EXPARAM_1671 u_1035)

   let f3_1038 x_DO_NOT_CARE_1815 x_DO_NOT_CARE_1816 u_1039 x_DO_NOT_CARE_1813 x_DO_NOT_CARE_1814 a_EXPARAM_1261 set_flag_f5_1677 s_f5_e_EXPARAM_1671 a_1040 =
     a_1040 set_flag_f5_1677 s_f5_e_EXPARAM_1671
       ((c5_COEFFICIENT_1272 * a_EXPARAM_1261) + c6_COEFFICIENT_1273)
       set_flag_f5_1677 s_f5_e_EXPARAM_1671
       (f2_1034 set_flag_f5_1677 s_f5_e_EXPARAM_1671 u_1039 set_flag_f5_1677
         s_f5_e_EXPARAM_1671
         ((c3_COEFFICIENT_1266 * a_EXPARAM_1261) + c4_COEFFICIENT_1267)
         set_flag_f5_1677 s_f5_e_EXPARAM_1671 a_1040)

   let f4_1041 x_DO_NOT_CARE_1811 x_DO_NOT_CARE_1812 u_1042 set_flag_f5_1677 s_f5_e_EXPARAM_1671 v_1043 =
     v_1043

   let f5_without_checking_1690 x_DO_NOT_CARE_1809 x_DO_NOT_CARE_1810 u_1045 x_DO_NOT_CARE_1807 x_DO_NOT_CARE_1808 e_EXPARAM_1254 set_flag_f5_1677 s_f5_e_EXPARAM_1671 e_1046 =
     let set_flag_f5_1677 = true
     in
     let s_f5_e_EXPARAM_1671 = e_EXPARAM_1254
     in
       e_1046 set_flag_f5_1677 s_f5_e_EXPARAM_1671
         ((c1_COEFFICIENT_1256 * e_EXPARAM_1254) + c2_COEFFICIENT_1257)
         set_flag_f5_1677 s_f5_e_EXPARAM_1671
         (f4_1041 set_flag_f5_1677 s_f5_e_EXPARAM_1671 u_1045)

   let rec f5_1044 x_DO_NOT_CARE_1694 x_DO_NOT_CARE_1695 u_1045 x_DO_NOT_CARE_1692 x_DO_NOT_CARE_1693 e_EXPARAM_1254 prev_set_flag_f5_1676 s_prev_f5_e_EXPARAM_1674 e_1046 =
     let u = if prev_set_flag_f5_1676 then
              if (-1 * 1) + (-s_prev_f5_e_EXPARAM_1674) >
                 (-1 * 1) + (-e_EXPARAM_1254) &&
                 (-1 * 1) + (-e_EXPARAM_1254) >= 0 then
                ()
              else
                let u_27527 = fail ()
                in
                  bot()
             else
               ()
                 in
            f5_without_checking_1690 x_DO_NOT_CARE_1694 x_DO_NOT_CARE_1695
              u_1045 x_DO_NOT_CARE_1692 x_DO_NOT_CARE_1693 e_EXPARAM_1254
              prev_set_flag_f5_1676 s_prev_f5_e_EXPARAM_1674 e_1046

   let main_1047 set_flag_f5_1677 s_f5_e_EXPARAM_1671 u_1048 =
     let zz_1032_1049 =
       f3_1038 set_flag_f5_1677 s_f5_e_EXPARAM_1671 u_1048 set_flag_f5_1677
         s_f5_e_EXPARAM_1671 c0_COEFFICIENT_1250 set_flag_f5_1677
         s_f5_e_EXPARAM_1671
         (f5_1044 set_flag_f5_1677 s_f5_e_EXPARAM_1671 u_1048)
     in
       ()

   let main =
     main_1047 false 0 ()
