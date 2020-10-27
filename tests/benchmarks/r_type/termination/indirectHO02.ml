
let rec bot _ = bot ()
let fail _ = assert false

   let rec c1_COEFFICIENT_1088 = 0
   let rec c0_COEFFICIENT_1087 = 0
   let id_1030 set_flag_f_1187 s_f_n_1182 x_1031 = x_1031

   let app_1032 x_DO_NOT_CARE_1219 x_DO_NOT_CARE_1220 h_EXPARAM_1090 x_DO_NOT_CARE_1217 x_DO_NOT_CARE_1218 h_1033 set_flag_f_1187 s_f_n_1182 v_1034 =
     h_1033 set_flag_f_1187 s_f_n_1182 () set_flag_f_1187 s_f_n_1182 v_1034

   let rec f_without_checking_1197 x_DO_NOT_CARE_1215 x_DO_NOT_CARE_1216 n_1036 set_flag_f_1187 s_f_n_1182 u_1037 =
     let set_flag_f_1187 = true
     in
     let s_f_n_1182 = n_1036
     in
       if n_1036 > 0 then
         app_1032 set_flag_f_1187 s_f_n_1182
           ((c0_COEFFICIENT_1087 * n_1036) + c1_COEFFICIENT_1088)
           set_flag_f_1187 s_f_n_1182
           (f_without_checking_1197 set_flag_f_1187 s_f_n_1182 (n_1036 - 1))
       else
         id_1030

   let rec f_1035 x_DO_NOT_CARE_1199 x_DO_NOT_CARE_1200 n_1036 prev_set_flag_f_1186 s_prev_f_n_1184 u_1037 =
     let u  =if prev_set_flag_f_1186 then
              let u_8614 = fail ()
              in
                bot()
             else () in
            f_without_checking_1197 x_DO_NOT_CARE_1199 x_DO_NOT_CARE_1200
              n_1036 prev_set_flag_f_1186 s_prev_f_n_1184 u_1037

   let main (r:int(*-:{v:Int | true}*)) =
     let set_flag_f_1187 = false in
     let s_f_n_1182 = 0 in
     f_1035 set_flag_f_1187 s_f_n_1182 r set_flag_f_1187 s_f_n_1182
       () set_flag_f_1187 s_f_n_1182 ()
