
let rec bot _ = bot ()
let fail _ = assert false

   let rec bin_without_checking_1089 x_DO_NOT_CARE_1095 x_DO_NOT_CARE_1096 x_DO_NOT_CARE_1097 n_1031 set_flag_bin_1077 s_bin_n_1072 s_bin_k_1073 k_1032 =
     let set_flag_bin_1077 = true
     in
     let s_bin_k_1073 = k_1032
     in
     let s_bin_n_1072 = n_1031
     in
       if n_1031 = 0 then
         1
       else
         if k_1032 <= 0 || k_1032 >= n_1031 then
           1
         else
           bin_without_checking_1089 set_flag_bin_1077 s_bin_n_1072
             s_bin_k_1073 (n_1031 - 1) set_flag_bin_1077 s_bin_n_1072
             s_bin_k_1073 (k_1032 - 1)
           +
           bin_without_checking_1089 set_flag_bin_1077 s_bin_n_1072
             s_bin_k_1073 (n_1031 - 1) set_flag_bin_1077 s_bin_n_1072
             s_bin_k_1073 k_1032

   let rec bin_1030 x_DO_NOT_CARE_1091 x_DO_NOT_CARE_1092 x_DO_NOT_CARE_1093 n_1031 prev_set_flag_bin_1076 s_prev_bin_n_1074 s_prev_bin_k_1075 k_1032 =
     let u = if prev_set_flag_bin_1076 then
              if ((0 * 1) + (0 * s_prev_bin_n_1074)) + (1 * s_prev_bin_k_1075)
                 > ((0 * 1) + (0 * n_1031)) + (1 * k_1032) &&
                 ((0 * 1) + (0 * n_1031)) + (1 * k_1032) >= 0 ||
                 ((0 * 1) + (0 * s_prev_bin_n_1074)) + (1 * s_prev_bin_k_1075)
                 >= ((0 * 1) + (0 * n_1031)) + (1 * k_1032) &&
                 (((0 * 1) + (1 * s_prev_bin_n_1074)) + (-s_prev_bin_k_1075) >
                  ((0 * 1) + (1 * n_1031)) + (-k_1032) &&
                  ((0 * 1) + (1 * n_1031)) + (-k_1032) >= 0) then
                ()
              else
                let u_6000 = fail ()
                in
                  bot()
             else () in
            bin_without_checking_1089 x_DO_NOT_CARE_1091 x_DO_NOT_CARE_1092
              x_DO_NOT_CARE_1093 n_1031 prev_set_flag_bin_1076
              s_prev_bin_n_1074 s_prev_bin_k_1075 k_1032

   let main_1033 set_flag_bin_1077 s_bin_n_1072 s_bin_k_1073 u_1034 n_1035 k_1036 =
       if n_1035 >= 0 && k_1036 >= 0 then
         bin_1030 set_flag_bin_1077 s_bin_n_1072 s_bin_k_1073 n_1035
           set_flag_bin_1077 s_bin_n_1072 s_bin_k_1073 k_1036
       else
         0
let main =
     main_1033 false 0 0 ()
