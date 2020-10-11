
let rec bot _ = bot ()
let fail _ = assert false

   let rec c5_COEFFICIENT_1082 = 0
   let rec c4_COEFFICIENT_1081 = 0
   let rec c3_COEFFICIENT_1080 = 0
   let rec c2_COEFFICIENT_1078 = 0
   let rec c1_COEFFICIENT_1077 = 0
   let rec c0_COEFFICIENT_1076 = 0

   let rec app_1030 x_DO_NOT_CARE_1216 x_DO_NOT_CARE_1217 f_EXPARAM_1084 x_DO_NOT_CARE_1214 x_DO_NOT_CARE_1215 f_1033 set_flag_down_1132 s_down_x_1129 x_1034 =
     f_1033 set_flag_down_1132 s_down_x_1129 x_1034

   let rec down_without_checking_1152 set_flag_down_1132 s_down_x_1129 x_1035 =
     let set_flag_down_1132 = true
     in
     let s_down_x_1129 = x_1035
     in
       if x_1035 = 0 then
         ()
       else
         down_without_checking_1152 set_flag_down_1132 s_down_x_1129
           (x_1035 - 1)

   let rec down_1031 prev_set_flag_down_1131 s_prev_down_x_1130 x_1035 =
     let u  =if prev_set_flag_down_1131 then
              if (0 * 1) + (1 * s_prev_down_x_1130) > (0 * 1) + (1 * x_1035) &&
                 (0 * 1) + (1 * x_1035) >= 0 then
                ()
              else
                let u_7494 = fail ()
                in
                  bot()
             else () in
            down_without_checking_1152 prev_set_flag_down_1131
              s_prev_down_x_1130 x_1035

   let rec up_1032 set_flag_down_1132 s_down_x_1129 x_1036 =
     if x_1036 = 0 then
       ()
     else
       up_1032 set_flag_down_1132 s_down_x_1129 (x_1036 + 1)

   let main (t1_1039:int(*-:{v:Int | true}*)) (t2_1040:int(*-:{v:Int | true}*)) =
       let set_flag_down_1132 = false in
       let s_down_x_1129 = 0 in
       if t1_1039 > 0 then
         app_1030 set_flag_down_1132 s_down_x_1129
           ((c3_COEFFICIENT_1080 * t2_1040) +
            ((c4_COEFFICIENT_1081 * t1_1039) + c5_COEFFICIENT_1082))
           set_flag_down_1132 s_down_x_1129 down_1031 set_flag_down_1132
           s_down_x_1129 t1_1039
       else
         if t2_1040 < 0 then
           app_1030 set_flag_down_1132 s_down_x_1129
             ((c0_COEFFICIENT_1076 * t2_1040) +
              ((c1_COEFFICIENT_1077 * t1_1039) + c2_COEFFICIENT_1078))
             set_flag_down_1132 s_down_x_1129 up_1032 set_flag_down_1132
             s_down_x_1129 t2_1040
         else
           ()
