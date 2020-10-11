
let rec bot _ = bot ()
let fail _ = assert false

   let rec c1_COEFFICIENT_1086 = 0
   let rec c0_COEFFICIENT_1085 = 0

   let rec succ_without_checking_1117 set_flag_succ_1093 s_succ_n_1090 n_1031 =
     let set_flag_succ_1093 = true
     in
     let s_succ_n_1090 = n_1031
     in
       n_1031 + 1

   let rec succ_1030 prev_set_flag_succ_1092 s_prev_succ_n_1091 n_1031 =
     let u = if prev_set_flag_succ_1092 then
              let u_1232 = fail ()
              in
                bot()
             else () in
            succ_without_checking_1117 prev_set_flag_succ_1092
              s_prev_succ_n_1091 n_1031


   let g_1032 x_DO_NOT_CARE_1200 x_DO_NOT_CARE_1201 r_EXPARAM_1088 x_DO_NOT_CARE_1198 x_DO_NOT_CARE_1199 r_1033 set_flag_succ_1093 s_succ_n_1090 a_1034 =
     r_1033 set_flag_succ_1093 s_succ_n_1090
       (r_1033 set_flag_succ_1093 s_succ_n_1090 a_1034)

   let rec f_1035 set_flag_succ_1093 s_succ_n_1090 n_1036 =
     if n_1036 = 0 then
       succ_1030
     else
       g_1032 set_flag_succ_1093 s_succ_n_1090
         ((c0_COEFFICIENT_1085 * n_1036) + c1_COEFFICIENT_1086)
         set_flag_succ_1093 s_succ_n_1090
         (f_1035 set_flag_succ_1093 s_succ_n_1090 (n_1036 - 1))

   let main (n_1038:int(*-:{v:Int | true}*)) (x_1039:int(*-:{v:Int | true}*)) =
     let x_DO_NOT_CARE_1196 = false in
     let x_DO_NOT_CARE_1197 = 0 in
     let set_flag_succ_1093 = false in
     let s_succ_n_1090 = 0 in
          if n_1038 >= 0 && x_1039 >= 0 then
       f_1035 set_flag_succ_1093 s_succ_n_1090 n_1038 set_flag_succ_1093
         s_succ_n_1090 x_1039
     else
       0
