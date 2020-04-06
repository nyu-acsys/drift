let main (l1_1036(*-:{v:Int | true}*)) (l2_1037(*-:{v:Int | true}*)) =
	let rec bot bx = bot () in
	let fail fx = assert (false) in

	let rec append_without_checking_1072 x_DO_NOT_CARE_1078 x_DO_NOT_CARE_1079 x_DO_NOT_CARE_1080 xs_10311 set_flag_append_1059 s_append_xs_1054 s_append_ys_1055 ys_10321 =
	 let set_flag_append_1059_r = true
	 in
	 let s_append_ys_1055_r = ys_10321
	 in
	 let s_append_xs_1054_r = xs_10311
	 in
	   if xs_10311 <= 0 then
	     ys_10321
	   else
	     let xs_1033_1 = xs_10311 - 1
	     in
	       1 +
	       append_without_checking_1072 set_flag_append_1059_r s_append_xs_1054_r
	         s_append_ys_1055_r xs_1033_1 set_flag_append_1059_r s_append_xs_1054_r
	         s_append_ys_1055_r ys_10321
	in

	let rec append_1030 append_without_checking_1072 x_DO_NOT_CARE_1074 x_DO_NOT_CARE_1075 x_DO_NOT_CARE_1076 xs_1031 prev_set_flag_append_1058 s_prev_append_xs_1056 s_prev_append_ys_1057 ys_1032 =
	 let u = if prev_set_flag_append_1058 then
	          if ((0 * 1) + (1 * s_prev_append_xs_1056)) +
	             (0 * s_prev_append_ys_1057) >
	             ((0 * 1) + (1 * xs_1031)) + (0 * ys_1032) &&
	             ((0 * 1) + (1 * xs_1031)) + (0 * ys_1032) >= 0 then
	            ()
	          else
	            let u_3245 = fail ()
	            in
	              bot()
	         else () in
	        append_without_checking_1072 x_DO_NOT_CARE_1074 x_DO_NOT_CARE_1075
	          x_DO_NOT_CARE_1076 xs_1031 prev_set_flag_append_1058
	          s_prev_append_xs_1056 s_prev_append_ys_1057 ys_1032
	in

	let main_1034 set_flag_append_1059 s_append_xs_1054 s_append_ys_1055 u_1035 =
	   append_without_checking_1072 set_flag_append_1059 s_append_xs_1054
	     s_append_ys_1055 l1_1036 set_flag_append_1059 s_append_xs_1054
	     s_append_ys_1055 l2_1037
	in

    assert(main_1034 false 0 0 () >= l2_1037)