
let main = 
    let rec bot bx = bot () in
    let fail fx = assert (false) in

    let c9_COEFFICIENT_1286 = 0 in
    let c8_COEFFICIENT_1285 = 0 in
    let c7_COEFFICIENT_1284 = 0 in
    let c6_COEFFICIENT_1273 = 0 in
    let c5_COEFFICIENT_1272 = 0 in
    let c4_COEFFICIENT_1267 = 0 in
    let c3_COEFFICIENT_1266 = 0 in
    let c2_COEFFICIENT_1257 = 0 in
    let c1_COEFFICIENT_1256 = 0 in
    let c0_COEFFICIENT_1250 = 0 in

    let f1_1030 x_DO_NOT_CARE_1793 x_DO_NOT_CARE_1794 u_1031 x_DO_NOT_CARE_1791 x_DO_NOT_CARE_1792 c_EXPARAM_1293 x_DO_NOT_CARE_1789 x_DO_NOT_CARE_1790 c_1032 set_flag_f3_1504hu s_f3_a_EXPARAM_1498fu d_1033 =
     d_1033
    in

    let f2_1034 x_DO_NOT_CARE_1787 x_DO_NOT_CARE_1788 u_1035 x_DO_NOT_CARE_1785 x_DO_NOT_CARE_1786 a_EXPARAM_1279 x_DO_NOT_CARE_1783 x_DO_NOT_CARE_1784 a_1036 x_DO_NOT_CARE_1781 x_DO_NOT_CARE_1782 b_EXPARAM_1282 set_flag_f3_1504 s_f3_a_EXPARAM_1498 b_1037 =
     a_1036 set_flag_f3_1504 s_f3_a_EXPARAM_1498
       ((c7_COEFFICIENT_1284 * b_EXPARAM_1282) +
        ((c8_COEFFICIENT_1285 * a_EXPARAM_1279) + c9_COEFFICIENT_1286))
       set_flag_f3_1504 s_f3_a_EXPARAM_1498
       (f1_1030 set_flag_f3_1504 s_f3_a_EXPARAM_1498 u_1035)
    in

    let f3_without_checking_1535 x_DO_NOT_CARE_1779 x_DO_NOT_CARE_1780 u_10397 x_DO_NOT_CARE_1777 x_DO_NOT_CARE_1778 a_EXPARAM_1261 set_flag_f3_1504bh s_f3_a_EXPARAM_1498gr a_10404 =
     let set_flag_f3_1504_r = true
     in
     let s_f3_a_EXPARAM_1498_r = a_EXPARAM_1261
     in
       a_10404 set_flag_f3_1504_r s_f3_a_EXPARAM_1498_r
         ((c5_COEFFICIENT_1272 * a_EXPARAM_1261) + c6_COEFFICIENT_1273)
         set_flag_f3_1504_r s_f3_a_EXPARAM_1498_r
         (f2_1034 set_flag_f3_1504_r s_f3_a_EXPARAM_1498_r u_10397 set_flag_f3_1504_r
           s_f3_a_EXPARAM_1498_r
           ((c3_COEFFICIENT_1266 * a_EXPARAM_1261) + c4_COEFFICIENT_1267)
           set_flag_f3_1504_r s_f3_a_EXPARAM_1498_r a_10404)
    in

    let rec f3_1038 x_DO_NOT_CARE_1539 x_DO_NOT_CARE_1540 u_1039 x_DO_NOT_CARE_1537 x_DO_NOT_CARE_1538 a_EXPARAM_1261p prev_set_flag_f3_1503 s_prev_f3_a_EXPARAM_1501 a_1040 =
     let u = if prev_set_flag_f3_1503 then
              let u_15524 = fail ()
              in
                bot()
             else () in
            f3_without_checking_1535 x_DO_NOT_CARE_1539 x_DO_NOT_CARE_1540
              u_1039 x_DO_NOT_CARE_1537 x_DO_NOT_CARE_1538 a_EXPARAM_1261p
              prev_set_flag_f3_1503 s_prev_f3_a_EXPARAM_1501 a_1040
    in

    let f4_1041 x_DO_NOT_CARE_1775 x_DO_NOT_CARE_1776 u_1042 set_flag_f3_1504fr s_f3_a_EXPARAM_149841 v_1043 =
     v_1043
    in

    let f5_1044 x_DO_NOT_CARE_1773 x_DO_NOT_CARE_1774 u_1045 x_DO_NOT_CARE_1771 x_DO_NOT_CARE_1772 e_EXPARAM_1254 set_flag_f3_1504f s_f3_a_EXPARAM_1498f e_1046 =
     e_1046 set_flag_f3_1504f s_f3_a_EXPARAM_1498f
       ((c1_COEFFICIENT_1256 * e_EXPARAM_1254) + c2_COEFFICIENT_1257)
       set_flag_f3_1504f s_f3_a_EXPARAM_1498f
       (f4_1041 set_flag_f3_1504f s_f3_a_EXPARAM_1498f u_1045)
    in

    let main_1047 set_flag_f3_1504m s_f3_a_EXPARAM_1498m u_1048 =
     let zz_1032_1049 =
       f3_1038 set_flag_f3_1504m s_f3_a_EXPARAM_1498m u_1048 set_flag_f3_1504m
         s_f3_a_EXPARAM_1498m c0_COEFFICIENT_1250 set_flag_f3_1504m
         s_f3_a_EXPARAM_1498m
         (f5_1044 set_flag_f3_1504m s_f3_a_EXPARAM_1498m u_1048)
     in
       ()
    in

     main_1047 false 0 ()
in main
(*7mins*)