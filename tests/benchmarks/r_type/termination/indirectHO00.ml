
let rec bot _ = bot ()
let fail _ = assert false

   let rec c1_COEFFICIENT_1088 = 0
   let rec c0_COEFFICIENT_1087 = 0

   let id_without_checking_1121 set_flag_id_1095 x_1031 =
     let set_flag_id_1095 = true
     in
       x_1031

   let rec id_1030 prev_set_flag_id_1094 x_1031 =
     let u =if prev_set_flag_id_1094 then
              let u_1229 = fail ()
              in
                bot()
            else () in
            id_without_checking_1121 prev_set_flag_id_1094 x_1031

   let app_1032 x_DO_NOT_CARE_1208 h_EXPARAM_1090 x_DO_NOT_CARE_1207 h_1033 set_flag_id_1095 v_1034 =
     h_1033 set_flag_id_1095 () set_flag_id_1095 v_1034

   let rec f_1035 x_DO_NOT_CARE_1206 n_1036 set_flag_id_1095 u_1037 =
     if n_1036 > 0 then
       app_1032 set_flag_id_1095
         ((c0_COEFFICIENT_1087 * n_1036) + c1_COEFFICIENT_1088)
         set_flag_id_1095 (f_1035 set_flag_id_1095 (n_1036 - 1))
     else
       id_1030

   let main (r:int(*-:{v:Int | true}*)) =
     let set_flag_id_1095 = false in
     f_1035 set_flag_id_1095 r set_flag_id_1095 () set_flag_id_1095
       ()
