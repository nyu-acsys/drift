let main (r(*-:{v:Int | true}*)) =
	let rec bot bx = bot () in
	let fail fx = assert (false) in

	let c1_COEFFICIENT_1085 = 0 in
	let c0_COEFFICIENT_1084 = 0 in

	let id_1030 set_flag_app_1137 s_app_h_EXPARAM_11280 s_app_v_11300 x_1031 =
	 x_1031
	in

	let rec app_without_checking_1157 x_DO_NOT_CARE_1215 x_DO_NOT_CARE_1216 x_DO_NOT_CARE_1217 h_EXPARAM_10877 x_DO_NOT_CARE_1212 x_DO_NOT_CARE_1213 x_DO_NOT_CARE_1214 h_10337 x_DO_NOT_CARE_1209 x_DO_NOT_CARE_1210 x_DO_NOT_CARE_1211 v_10347 set_flag_app_1137 s_app_h_EXPARAM_1128 s_app_v_1130 u_10357 =
	 let set_flag_app_1137_r = true
	 in
	 let s_app_v_1130_r = v_10347
	 in
	 let s_app_h_EXPARAM_1128_r = h_EXPARAM_10877
	 in
	   h_10337 set_flag_app_1137_r s_app_h_EXPARAM_1128_r s_app_v_1130_r v_10347
	     set_flag_app_1137_r s_app_h_EXPARAM_1128_r s_app_v_1130_r u_10357
	in

	let rec app_1032 x_DO_NOT_CARE_1165 x_DO_NOT_CARE_1166 x_DO_NOT_CARE_1167 h_EXPARAM_1087 x_DO_NOT_CARE_1162 x_DO_NOT_CARE_1163 x_DO_NOT_CARE_1164 h_1033 x_DO_NOT_CARE_1159 x_DO_NOT_CARE_1160 x_DO_NOT_CARE_1161 v_1034 prev_set_flag_app_1136 s_prev_app_h_EXPARAM_1132 s_prev_app_v_1134 u_1035 =
	 let u = if prev_set_flag_app_1136 then
	          if ((0 * 1) + (0 * s_prev_app_h_EXPARAM_1132)) +
	             (1 * s_prev_app_v_1134) >
	             ((0 * 1) + (0 * h_EXPARAM_1087)) + (1 * v_1034) &&
	             ((0 * 1) + (0 * h_EXPARAM_1087)) + (1 * v_1034) >= 0 then
	            ()
	          else
	            let u_5655 = fail ()
	            in
	              bot()
	         else () in
	  app_without_checking_1157 x_DO_NOT_CARE_1165 x_DO_NOT_CARE_1166
	    x_DO_NOT_CARE_1167 h_EXPARAM_1087 x_DO_NOT_CARE_1162
	    x_DO_NOT_CARE_1163 x_DO_NOT_CARE_1164 h_1033 x_DO_NOT_CARE_1159
	    x_DO_NOT_CARE_1160 x_DO_NOT_CARE_1161 v_1034
	    prev_set_flag_app_1136 s_prev_app_h_EXPARAM_1132
	    s_prev_app_v_1134 u_1035
	in

	let rec f_1036 set_flag_app_1137 s_app_h_EXPARAM_1128 s_app_v_1130 x_1037 =
	 if x_1037 > 0 then
	   app_1032 set_flag_app_1137 s_app_h_EXPARAM_1128 s_app_v_1130
	     ((c0_COEFFICIENT_1084 * x_1037) + c1_COEFFICIENT_1085)
	     set_flag_app_1137 s_app_h_EXPARAM_1128 s_app_v_1130 f_1036
	     set_flag_app_1137 s_app_h_EXPARAM_1128 s_app_v_1130 (x_1037 - 1)
	 else
	   id_1030
	in


	 let set_flag_app_1137m = false in
	 let s_app_h_EXPARAM_1128m = 0 in
	 let s_app_v_1130m = 0 in
	 f_1036 set_flag_app_1137m s_app_h_EXPARAM_1128m s_app_v_1130m r
	   set_flag_app_1137m s_app_h_EXPARAM_1128m s_app_v_1130m ()

