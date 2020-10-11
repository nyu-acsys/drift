
let rec bot _ = bot ()
let fail _ = assert false

   let rec c3_COEFFICIENT_1089 = 0
   let rec c2_COEFFICIENT_1088 = 0
   let rec c1_COEFFICIENT_1085 = 0
   let rec c0_COEFFICIENT_1084 = 0

   let id_1030 set_flag_f_1191 s_f_x_EXPARAM_1180 s_f_y_EXPARAM_1182 s_f_z_1184 x_1031 =
     x_1031

   let rec omega_1032 set_flag_f_1191 s_f_x_EXPARAM_1180 s_f_y_EXPARAM_1182 s_f_z_1184 x_1033 =
     omega_1032 set_flag_f_1191 s_f_x_EXPARAM_1180 s_f_y_EXPARAM_1182
       s_f_z_1184 x_1033

   let rec f_without_checking_1206 x_DO_NOT_CARE_1259 x_DO_NOT_CARE_1260 x_DO_NOT_CARE_1261 x_DO_NOT_CARE_1262 x_EXPARAM_1092 x_DO_NOT_CARE_1255 x_DO_NOT_CARE_1256 x_DO_NOT_CARE_1257 x_DO_NOT_CARE_1258 x_1035 x_DO_NOT_CARE_1251 x_DO_NOT_CARE_1252 x_DO_NOT_CARE_1253 x_DO_NOT_CARE_1254 y_EXPARAM_1093 x_DO_NOT_CARE_1247 x_DO_NOT_CARE_1248 x_DO_NOT_CARE_1249 x_DO_NOT_CARE_1250 y_1036 set_flag_f_1191 s_f_x_EXPARAM_1180 s_f_y_EXPARAM_1182 s_f_z_1184 z_1037 =
     let set_flag_f_1191 = true
     in
     let s_f_z_1184 = z_1037
     in
     let s_f_y_EXPARAM_1182 = y_EXPARAM_1093
     in
     let s_f_x_EXPARAM_1180 = x_EXPARAM_1092
     in
       y_1036 set_flag_f_1191 s_f_x_EXPARAM_1180 s_f_y_EXPARAM_1182 s_f_z_1184
         z_1037

   let rec f_1034 x_DO_NOT_CARE_1220 x_DO_NOT_CARE_1221 x_DO_NOT_CARE_1222 x_DO_NOT_CARE_1223 x_EXPARAM_1092 x_DO_NOT_CARE_1216 x_DO_NOT_CARE_1217 x_DO_NOT_CARE_1218 x_DO_NOT_CARE_1219 x_1035 x_DO_NOT_CARE_1212 x_DO_NOT_CARE_1213 x_DO_NOT_CARE_1214 x_DO_NOT_CARE_1215 y_EXPARAM_1093 x_DO_NOT_CARE_1208 x_DO_NOT_CARE_1209 x_DO_NOT_CARE_1210 x_DO_NOT_CARE_1211 y_1036 prev_set_flag_f_1190 s_prev_f_x_EXPARAM_1185 s_prev_f_y_EXPARAM_1187 s_prev_f_z_1189 z_1037 =
     let u = if prev_set_flag_f_1190 then
              let u_5975 = fail ()
              in
                bot()
             else () in
            f_without_checking_1206 x_DO_NOT_CARE_1220 x_DO_NOT_CARE_1221
              x_DO_NOT_CARE_1222 x_DO_NOT_CARE_1223 x_EXPARAM_1092
              x_DO_NOT_CARE_1216 x_DO_NOT_CARE_1217 x_DO_NOT_CARE_1218
              x_DO_NOT_CARE_1219 x_1035 x_DO_NOT_CARE_1212 x_DO_NOT_CARE_1213
              x_DO_NOT_CARE_1214 x_DO_NOT_CARE_1215 y_EXPARAM_1093
              x_DO_NOT_CARE_1208 x_DO_NOT_CARE_1209 x_DO_NOT_CARE_1210
              x_DO_NOT_CARE_1211 y_1036 prev_set_flag_f_1190
              s_prev_f_x_EXPARAM_1185 s_prev_f_y_EXPARAM_1187 s_prev_f_z_1189
              z_1037

   let main =
     f_without_checking_1206 false 0 0 0 c2_COEFFICIENT_1088 false 0 0 0
       (f_1034 false 0 0 0 c0_COEFFICIENT_1084 false 0 0 0 id_1030 false 0 0 0
         c1_COEFFICIENT_1085 false 0 0 0 omega_1032) false 0 0 0
       c3_COEFFICIENT_1089 false 0 0 0 id_1030 false 0 0 0 1
