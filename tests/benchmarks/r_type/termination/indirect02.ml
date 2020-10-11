
let rec bot _ = bot ()
let fail _ = assert false

   let rec c1_COEFFICIENT_1085 = 0
   let rec c0_COEFFICIENT_1084 = 0

   let id_1030 set_flag_f_1191 s_f_x_1188 x_1031 = x_1031

   let app_1032 x_DO_NOT_CARE_1222 x_DO_NOT_CARE_1223 h_EXPARAM_1087 x_DO_NOT_CARE_1220 x_DO_NOT_CARE_1221 h_1033 x_DO_NOT_CARE_1218 x_DO_NOT_CARE_1219 v_1034 set_flag_f_1191 s_f_x_1188 u_1035 =
     h_1033 set_flag_f_1191 s_f_x_1188 v_1034 set_flag_f_1191 s_f_x_1188 u_1035

   let rec f_without_checking_1199 set_flag_f_1191 s_f_x_1188 x_1037 =
     let set_flag_f_1191 = true
     in
     let s_f_x_1188 = x_1037
     in
       if x_1037 > 0 then
         app_1032 set_flag_f_1191 s_f_x_1188
           ((c0_COEFFICIENT_1084 * x_1037) + c1_COEFFICIENT_1085)
           set_flag_f_1191 s_f_x_1188 f_without_checking_1199 set_flag_f_1191
           s_f_x_1188 (x_1037 - 1)
       else
         id_1030

   let rec f_1036 prev_set_flag_f_1190 s_prev_f_x_1189 x_1037 =
     let u = if prev_set_flag_f_1190 then
              let u_10809 = fail ()
              in
                bot()
             else () in
            f_without_checking_1199 prev_set_flag_f_1190 s_prev_f_x_1189 x_1037

   let main (r:int(*-:{v:Int | true}*)) =
     let set_flag_f_1191 = false in
     let s_f_x_1188 = 0 in
     f_1036 set_flag_f_1191 s_f_x_1188 r set_flag_f_1191 s_f_x_1188
     ()
