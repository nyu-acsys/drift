
let rec bot bx = bot ()
let fail fx = assert (false)

let c1_COEFFICIENT_1086 = 0
let c0_COEFFICIENT_1085 = 0

let succ_1030 set_flag_f_11810 s_f_n_11780 n_1031 = n_1031 + 1

let g_1032 x_DO_NOT_CARE_1215 x_DO_NOT_CARE_1216 r_EXPARAM_1088 x_DO_NOT_CARE_1213 x_DO_NOT_CARE_1214 r_1033 set_flag_f_11812 s_f_n_11782 a_1034 =
  r_1033 set_flag_f_11812 s_f_n_11782
    (r_1033 set_flag_f_11812 s_f_n_11782 a_1034)

let rec f_without_checking_1189 set_flag_f_1181 s_f_n_1178 n_1036 =
  let set_flag_f_1181_r = true
  in
  let s_f_n_1178_r = n_1036
  in
    if n_1036 = 0 then
      succ_1030
    else
      g_1032 set_flag_f_1181_r s_f_n_1178_r
        ((c0_COEFFICIENT_1085 * n_1036) + c1_COEFFICIENT_1086)
        set_flag_f_1181_r s_f_n_1178_r
        (f_without_checking_1189 set_flag_f_1181_r s_f_n_1178_r (n_1036 - 1))

let rec f_1035 prev_set_flag_f_1180 s_prev_f_n_1179 n_10365 =
  let u = if prev_set_flag_f_1180 then
           let u_11903 = fail ()
           in
             bot()
          else () in
         f_without_checking_1189 prev_set_flag_f_1180 s_prev_f_n_1179 n_10365

let main n_1038 x_1039 =
  let x_DO_NOT_CARE_1211 = false in
  let x_DO_NOT_CARE_1212 = 0 in
  let set_flag_f_1181m = false in
  let s_f_n_1178m = 0 in
  let res = if n_1038 >= 0 && x_1039 >= 0 then
    f_1035 set_flag_f_1181m s_f_n_1178m n_1038 set_flag_f_1181m s_f_n_1178m
      x_1039
  else
    0
  in assert (res >= 0)