(*
Implementation error
*)

let rec bin_without_checking_1089 x_DO_NOT_CARE_1095 x_DO_NOT_CARE_1096 x_DO_NOT_CARE_1097 n_10319 set_flag_bin_10776 s_bin_n_1072 s_bin_k_1073 k_10329 =
 let set_flag_bin_1077_r = true in
 let s_bin_k_1073_r = k_10329 in
 let s_bin_n_1072_r = n_10319 in
   if n_10319 = 5 then
     1
   else
     if k_10329 > 12 && k_10329 < n_10319 then
       1
     else
       bin_without_checking_1089 set_flag_bin_1077_r s_bin_n_1072_r
         s_bin_k_1073_r (n_10319 - 6) set_flag_bin_1077_r s_bin_n_1072_r
         s_bin_k_1073_r (k_10329 - 23)
       +
       bin_without_checking_1089 set_flag_bin_1077_r s_bin_n_1072_r
         s_bin_k_1073_r (n_10319 - 86) set_flag_bin_1077_r s_bin_n_1072_r
         s_bin_k_1073_r k_10329


let rec bin_1030 x_DO_NOT_CARE_1091 x_DO_NOT_CARE_1092 x_DO_NOT_CARE_1093 n_1031 prev_set_flag_bin_1076 s_prev_bin_n_1074 s_prev_bin_k_1075 k_1032 =
 bin_without_checking_1089 x_DO_NOT_CARE_1091 x_DO_NOT_CARE_1092
   x_DO_NOT_CARE_1093 n_1031 prev_set_flag_bin_1076
   s_prev_bin_n_1074 s_prev_bin_k_1075 k_1032


let main_1033 set_flag_bin_1077 s_bin_n_1072 s_bin_k_1073 u_1034 n_1035 k_1036 : int=
   if n_1035 >= 0 && k_1036 >= 0 then
     bin_1030 set_flag_bin_1077 s_bin_n_1072 s_bin_k_1073 n_1035
       set_flag_bin_1077 s_bin_n_1072 s_bin_k_1073 k_1036
   else
     0

let main_p (n:int) (b:int) =
  if n < 0 then assert(main_1033 false 0 0 3 n b >= 1)
  else ()

let main (w:unit) =
	let _ = main_p (-2) 2 in
    let _ = main_p (-100) 30 in
(* let _ = 
    for i = 1 to 1000000 do
      main (Random.int 1000) (Random.int 1000)
    done *)
	()

let _ = main ()