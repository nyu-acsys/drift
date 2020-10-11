
let rec bot _ = bot ()
let fail _ = assert false

   let rec c1_COEFFICIENT_1086 = 0
   let rec c0_COEFFICIENT_1085 = 0
   let succ_1030 set_flag_f_1181 s_f_n_1178 n_1031 = n_1031 + 1

   let g_1032 x_DO_NOT_CARE_1215 x_DO_NOT_CARE_1216 r_EXPARAM_1088 x_DO_NOT_CARE_1213 x_DO_NOT_CARE_1214 r_1033 set_flag_f_1181 s_f_n_1178 a_1034 =
     r_1033 set_flag_f_1181 s_f_n_1178
       (r_1033 set_flag_f_1181 s_f_n_1178 a_1034)

   let rec f_1035 f_without_checking_1189 prev_set_flag_f_1180 s_prev_f_n_1179 n_1036 =
     let u = if prev_set_flag_f_1180 then
              if (0 * 1) + (1 * s_prev_f_n_1179) > (0 * 1) + (1 * n_1036) &&
                 (0 * 1) + (1 * n_1036) >= 0 then
                ()
              else
                let u_15618 = fail ()
                in
                  bot()
     else ()
       in
            f_without_checking_1189 prev_set_flag_f_1180 s_prev_f_n_1179 n_1036

   let rec f_without_checking_1189 set_flag_f_1181 s_f_n_1178 n_1036 =
     let set_flag_f_1181 = true
     in
     let s_f_n_1178 = n_1036
     in
       if n_1036 = 0 then
         succ_1030
       else
         g_1032 set_flag_f_1181 s_f_n_1178
           ((c0_COEFFICIENT_1085 * n_1036) + c1_COEFFICIENT_1086)
           set_flag_f_1181 s_f_n_1178
           (f_1035 f_without_checking_1189 set_flag_f_1181 s_f_n_1178 (n_1036 - 1))

   let main (n_1038:int(*-:{v:Int | true}*)) (x_1039:int(*-:{v:Int | true}*)) =
     let x_DO_NOT_CARE_1211 = false in
     let x_DO_NOT_CARE_1212 = 0 in
     let set_flag_f_1181 = false in
     let s_f_n_1178 = 0 in
     if n_1038 >= 0 && x_1039 >= 0 then
       f_without_checking_1189 set_flag_f_1181 s_f_n_1178 n_1038
         set_flag_f_1181 s_f_n_1178 x_1039
     else
       0
