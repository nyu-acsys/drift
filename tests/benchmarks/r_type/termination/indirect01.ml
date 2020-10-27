
let rec bot _ = bot ()
let fail _ = assert false

   let rec c1_COEFFICIENT_1085 = 0
   let rec c0_COEFFICIENT_1084 = 0
   let id_1030 set_flag_app_1137 s_app_h_EXPARAM_1128 s_app_v_1130 x_1031 =
     x_1031

   let rec app_without_checking_1157 x_DO_NOT_CARE_1215 x_DO_NOT_CARE_1216 x_DO_NOT_CARE_1217 h_EXPARAM_1087 x_DO_NOT_CARE_1212 x_DO_NOT_CARE_1213 x_DO_NOT_CARE_1214 h_1033 x_DO_NOT_CARE_1209 x_DO_NOT_CARE_1210 x_DO_NOT_CARE_1211 v_1034 set_flag_app_1137 s_app_h_EXPARAM_1128 s_app_v_1130 u_1035 =
     let set_flag_app_1137 = true
     in
     let s_app_v_1130 = v_1034
     in
     let s_app_h_EXPARAM_1128 = h_EXPARAM_1087
     in
       h_1033 set_flag_app_1137 s_app_h_EXPARAM_1128 s_app_v_1130 v_1034
         set_flag_app_1137 s_app_h_EXPARAM_1128 s_app_v_1130 u_1035

   let rec app_1032 x_DO_NOT_CARE_1165 x_DO_NOT_CARE_1166 x_DO_NOT_CARE_1167 h_EXPARAM_1087 x_DO_NOT_CARE_1162 x_DO_NOT_CARE_1163 x_DO_NOT_CARE_1164 h_1033 x_DO_NOT_CARE_1159 x_DO_NOT_CARE_1160 x_DO_NOT_CARE_1161 v_1034 prev_set_flag_app_1136 s_prev_app_h_EXPARAM_1132 s_prev_app_v_1134 u_1035 =
     let u = if prev_set_flag_app_1136 then
              if ((0 * 1) + (0 * s_prev_app_h_EXPARAM_1132)) +
                 (1 * s_prev_app_v_1134) >
                 ((0 * 1) + (0 * h_EXPARAM_1087)) + (1 * v_1034) &&
                 ((0 * 1) + (0 * h_EXPARAM_1087)) + (1 * v_1034) >= 0 then
                ()
              else
                let u_5655 = fail ()
                in
                  bot()
             else () in
            app_without_checking_1157 x_DO_NOT_CARE_1165 x_DO_NOT_CARE_1166
              x_DO_NOT_CARE_1167 h_EXPARAM_1087 x_DO_NOT_CARE_1162
              x_DO_NOT_CARE_1163 x_DO_NOT_CARE_1164 h_1033 x_DO_NOT_CARE_1159
              x_DO_NOT_CARE_1160 x_DO_NOT_CARE_1161 v_1034
              prev_set_flag_app_1136 s_prev_app_h_EXPARAM_1132
              s_prev_app_v_1134 u_1035

   let rec f_1036 set_flag_app_1137 s_app_h_EXPARAM_1128 s_app_v_1130 x_1037 =
     if x_1037 > 0 then
       app_1032 set_flag_app_1137 s_app_h_EXPARAM_1128 s_app_v_1130
         ((c0_COEFFICIENT_1084 * x_1037) + c1_COEFFICIENT_1085)
         set_flag_app_1137 s_app_h_EXPARAM_1128 s_app_v_1130 f_1036
         set_flag_app_1137 s_app_h_EXPARAM_1128 s_app_v_1130 (x_1037 - 1)
     else
       id_1030

   let main (r:int(*-:{v:Int | true}*)) =
     let set_flag_app_1137 = false in
     let s_app_h_EXPARAM_1128 = 0 in
     let s_app_v_1130 = 0 in
     f_1036 set_flag_app_1137 s_app_h_EXPARAM_1128 s_app_v_1130 r
       set_flag_app_1137 s_app_h_EXPARAM_1128 s_app_v_1130 ()
