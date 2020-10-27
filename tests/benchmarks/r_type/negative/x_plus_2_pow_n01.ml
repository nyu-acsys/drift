
let rec bot _ = bot ()
let fail _ = assert false


   let rec c1_COEFFICIENT_1086 = 0
   let rec c0_COEFFICIENT_1085 = 0

   let succ_1030 set_flag_g_1134 s_g_r_EXPARAM_1127 s_g_a_1129 n_1031 =
     n_1031 + 1

   let rec g_without_checking_1152 x_DO_NOT_CARE_1208 x_DO_NOT_CARE_1209 x_DO_NOT_CARE_1210 r_EXPARAM_1088 x_DO_NOT_CARE_1205 x_DO_NOT_CARE_1206 x_DO_NOT_CARE_1207 r_1033 set_flag_g_1134 s_g_r_EXPARAM_1127 s_g_a_1129 a_1034 =
     let set_flag_g_1134 = true
     in
     let s_g_a_1129 = a_1034
     in
     let s_g_r_EXPARAM_1127 = r_EXPARAM_1088
     in
       r_1033 set_flag_g_1134 s_g_r_EXPARAM_1127 s_g_a_1129
         (r_1033 set_flag_g_1134 s_g_r_EXPARAM_1127 s_g_a_1129 a_1034)

   let rec g_1032 x_DO_NOT_CARE_1157 x_DO_NOT_CARE_1158 x_DO_NOT_CARE_1159 r_EXPARAM_1088 x_DO_NOT_CARE_1154 x_DO_NOT_CARE_1155 x_DO_NOT_CARE_1156 r_1033 prev_set_flag_g_1133 s_prev_g_r_EXPARAM_1130 s_prev_g_a_1132 a_1034 =
     let u =  if prev_set_flag_g_1133 then
              if ((2 * 1) + (-4 * s_prev_g_r_EXPARAM_1130)) +
                 (0 * s_prev_g_a_1132) >
                 ((2 * 1) + (-4 * r_EXPARAM_1088)) + (0 * a_1034) &&
                 ((2 * 1) + (-4 * r_EXPARAM_1088)) + (0 * a_1034) >= 0 then
                ()
              else
                let u_5939 = fail ()
                in
                  bot()
              else () in
            g_without_checking_1152 x_DO_NOT_CARE_1157 x_DO_NOT_CARE_1158
              x_DO_NOT_CARE_1159 r_EXPARAM_1088 x_DO_NOT_CARE_1154
              x_DO_NOT_CARE_1155 x_DO_NOT_CARE_1156 r_1033 prev_set_flag_g_1133
              s_prev_g_r_EXPARAM_1130 s_prev_g_a_1132 a_1034

   let rec f_1035 set_flag_g_1134 s_g_r_EXPARAM_1127 s_g_a_1129 n_1036 =
     if n_1036 = 0 then
       succ_1030
     else
       g_1032 set_flag_g_1134 s_g_r_EXPARAM_1127 s_g_a_1129
         ((c0_COEFFICIENT_1085 * n_1036) + c1_COEFFICIENT_1086) set_flag_g_1134
         s_g_r_EXPARAM_1127 s_g_a_1129
         (f_1035 set_flag_g_1134 s_g_r_EXPARAM_1127 s_g_a_1129 (n_1036 - 1))

   let main (n_1038:int(*-:{v:Int | true}*)) (x_1039:int(*-:{v:Int | true}*)) =
     let x_DO_NOT_CARE_1202 = false in
     let x_DO_NOT_CARE_1203 = 0 in
     let x_DO_NOT_CARE_1204 = 0 in
     let set_flag_g_1134 = false in
     let s_g_r_EXPARAM_1127 = 0 in
     let s_g_a_1129 = 0 in
     if n_1038 >= 0 && x_1039 >= 0 then
       f_1035 set_flag_g_1134 s_g_r_EXPARAM_1127 s_g_a_1129 n_1038
         set_flag_g_1134 s_g_r_EXPARAM_1127 s_g_a_1129 x_1039
     else
       0
