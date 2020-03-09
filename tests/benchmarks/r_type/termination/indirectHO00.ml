let main r =
let rec bot bx = bot () in
let fail fx = assert (false) in

let c1_COEFFICIENT_1088 = 0 in
let c0_COEFFICIENT_1087 = 0 in

let id_without_checking_1121 set_flag_id_10951 x_1031 =
 let set_flag_id_1095_r = true in
   x_1031
in

let rec id_1030 prev_set_flag_id_1094 x_10311 =
  let u =if prev_set_flag_id_1094 then
    let u_1229 = fail () in
    bot()
  else () in
  id_without_checking_1121 prev_set_flag_id_1094 x_10311
in

let app_1032 x_DO_NOT_CARE_1208 h_EXPARAM_1090 x_DO_NOT_CARE_1207 h_1033 set_flag_id_10952 v_1034 =
 h_1033 set_flag_id_10952 () set_flag_id_10952 v_1034
in

let rec f_1035 x_DO_NOT_CARE_1206 n_1036 set_flag_id_1095 u_1037 =
 if n_1036 > 0 then
   app_1032 set_flag_id_1095
     ((c0_COEFFICIENT_1087 * n_1036) + c1_COEFFICIENT_1088)
     set_flag_id_1095 (f_1035 set_flag_id_1095 (n_1036 - 1))
 else
   id_1030
in


 let set_flag_id_1095m = false in
 f_1035 set_flag_id_1095m r set_flag_id_1095m () set_flag_id_1095m
   ()
in main 46