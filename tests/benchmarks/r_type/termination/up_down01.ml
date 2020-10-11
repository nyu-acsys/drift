
let rec bot _ = bot ()
let fail _ = assert false

   let rec c5_COEFFICIENT_1082 = 0
   let rec c4_COEFFICIENT_1081 = 0
   let rec c3_COEFFICIENT_1080 = 0
   let rec c2_COEFFICIENT_1078 = 0
   let rec c1_COEFFICIENT_1077 = 0
   let rec c0_COEFFICIENT_1076 = 0

   let rec app_1030 x_DO_NOT_CARE_1212 x_DO_NOT_CARE_1213 f_EXPARAM_1084 x_DO_NOT_CARE_1210 x_DO_NOT_CARE_1211 f_1033 set_flag_up_1089 s_up_x_1086 x_1034 =
     f_1033 set_flag_up_1089 s_up_x_1086 x_1034

   let rec down_1031 set_flag_up_1089 s_up_x_1086 x_1035 =
     if x_1035 = 0 then
       ()
     else
       down_1031 set_flag_up_1089 s_up_x_1086 (x_1035 - 1)

   let rec up_without_checking_1115 set_flag_up_1089 s_up_x_1086 x_1036 =
     let set_flag_up_1089 = true
     in
     let s_up_x_1086 = x_1036
     in
       if x_1036 = 0 then
         ()
       else
         up_without_checking_1115 set_flag_up_1089 s_up_x_1086 (x_1036 + 1)

   let rec up_1032 prev_set_flag_up_1088 s_prev_up_x_1087 x_1036 =
     let u = if prev_set_flag_up_1088 then
              if (0 * 1) + (-s_prev_up_x_1087) > (0 * 1) + (-x_1036) &&
                 (0 * 1) + (-x_1036) >= 0 then
                ()
              else
                let u_3056 = fail ()
                in
                  bot()
             else () in
            up_without_checking_1115 prev_set_flag_up_1088 s_prev_up_x_1087
              x_1036

   let main (t1_1039:int(*-:{v:Int | true}*)) (t2_1040:int(*-:{v:Int | true}*)) =
     let set_flag_up_1089 = false in
     let s_up_x_1086 = 0 in
       if t1_1039 > 0 then
         app_1030 set_flag_up_1089 s_up_x_1086
           ((c3_COEFFICIENT_1080 * t2_1040) +
            ((c4_COEFFICIENT_1081 * t1_1039) + c5_COEFFICIENT_1082))
           set_flag_up_1089 s_up_x_1086 down_1031 set_flag_up_1089 s_up_x_1086
           t1_1039
       else
         if t2_1040 < 0 then
           app_1030 set_flag_up_1089 s_up_x_1086
             ((c0_COEFFICIENT_1076 * t2_1040) +
              ((c1_COEFFICIENT_1077 * t1_1039) + c2_COEFFICIENT_1078))
             set_flag_up_1089 s_up_x_1086 up_1032 set_flag_up_1089 s_up_x_1086
             t2_1040
         else
           ()
