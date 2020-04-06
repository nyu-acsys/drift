
let rec bot bx = bot ()
let fail fx = assert (false)

let c5_COEFFICIENT_1082 = 0
let c4_COEFFICIENT_1081 = 0
let c3_COEFFICIENT_1080 = 0
let c2_COEFFICIENT_1078 = 0
let c1_COEFFICIENT_1077 = 0
let c0_COEFFICIENT_1076 = 0

let rec app_1030 x_DO_NOT_CARE_1212 x_DO_NOT_CARE_1213 f_EXPARAM_1084 x_DO_NOT_CARE_1210 x_DO_NOT_CARE_1211 f_1033 set_flag_up_10890 s_up_x_10860 x_1034 =
 f_1033 set_flag_up_10890 s_up_x_10860 x_1034

let rec down_1031 set_flag_up_10891 s_up_x_10861 x_1035 =
 if x_1035 = 0 then
   ()
 else
   down_1031 set_flag_up_10891 s_up_x_10861 (x_1035 - 1)

let rec up_1032 up_without prev_set_flag_up_1088 s_prev_up_x_1087 x_1036 =
 let u = if prev_set_flag_up_1088 then
          if (0 * 1) + (0-s_prev_up_x_1087) > (0 * 1) + (0-x_1036) &&
             (0 * 1) + (0-x_1036) >= 0 then
            ()
          else
            let u_3968 = fail ()
            in
              bot()
         else () in
        up_without prev_set_flag_up_1088 s_prev_up_x_1087
          x_1036

let rec up_without_checking_1115 set_flag_up_10895 s_up_x_10865 x_10365 =
 let set_flag_up_1089_r = true in
 let s_up_x_1086_r = x_10365
 in
   if x_10365 = 0 then
     ()
   else
     up_without_checking_1115 set_flag_up_1089_r s_up_x_1086_r (x_10365 + 1)

let main t1_1039 t2_1040 =
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
         set_flag_up_1089 s_up_x_1086 (up_1032 up_without_checking_1115) set_flag_up_1089 s_up_x_1086
         t2_1040
     else
       ()
