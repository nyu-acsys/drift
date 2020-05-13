(*
Implementation error
*)

let c1_COEFFICIENT_1088 = 0 
let c0_COEFFICIENT_1087 = 0 

let id_1030 set_flag_app_11380 s_app_h_EXPARAM_11310 x_1031 = (0 - x_1031)

let app_without_checking_1158 x_DO_NOT_CARE_1213 x_DO_NOT_CARE_1214 h_EXPARAM_10908 x_DO_NOT_CARE_1211 x_DO_NOT_CARE_1212 h_10330 set_flag_app_11380 s_app_h_EXPARAM_11310 v_10340 =
 let set_flag_app_1138_r = true
 in
 let s_app_h_EXPARAM_1131_r = h_EXPARAM_10908
 in
   h_10330 set_flag_app_1138_r s_app_h_EXPARAM_1131_r () set_flag_app_1138_r
     s_app_h_EXPARAM_1131_r v_10340

let rec app_1032 x_DO_NOT_CARE_1162 x_DO_NOT_CARE_1163 h_EXPARAM_1090 x_DO_NOT_CARE_1160 x_DO_NOT_CARE_1161 h_1033 prev_set_flag_app_1137 s_prev_app_h_EXPARAM_1134 v_1034 =
        app_without_checking_1158 x_DO_NOT_CARE_1162 x_DO_NOT_CARE_1163
          h_EXPARAM_1090 x_DO_NOT_CARE_1160 x_DO_NOT_CARE_1161 h_1033
          prev_set_flag_app_1137 s_prev_app_h_EXPARAM_1134 v_1034

let rec f_1035 x_DO_NOT_CARE_1209 x_DO_NOT_CARE_1210 n_1036 set_flag_app_1138 s_app_h_EXPARAM_1131 u_1037 =
 if n_1036 > 0 then
   app_1032 set_flag_app_1138 s_app_h_EXPARAM_1131
     ((c0_COEFFICIENT_1087 * n_1036) + c1_COEFFICIENT_1088)
     set_flag_app_1138 s_app_h_EXPARAM_1131
     (f_1035 set_flag_app_1138 s_app_h_EXPARAM_1131 (n_1036 - 1))
 else
   id_1030

let main_p (r:int) =
 let set_flag_app_1138m = false in
 let s_app_h_EXPARAM_1131m = 0 in
 if r > 0 then assert (f_1035 set_flag_app_1138m s_app_h_EXPARAM_1131m 0
   set_flag_app_1138m s_app_h_EXPARAM_1131m () set_flag_app_1138m
   s_app_h_EXPARAM_1131m r = r)
 else ()

let main (w:unit) =
	let _ = main_p 1234 in
    let _ = main_p 3 in
(* let _ = 
    for i = 1 to 1000000 do
      main (Random.int 1000)
    done *)
	()

let _ = main ()