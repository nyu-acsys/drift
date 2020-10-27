
let rec bot _ = bot ()
let fail _ = assert false

   let rec zip_without_checking_1077 x_DO_NOT_CARE_1083 x_DO_NOT_CARE_1084 x_DO_NOT_CARE_1085 xs_1031 set_flag_zip_1063 s_zip_xs_1058 s_zip_ys_1059 ys_1032 =
     let set_flag_zip_1063 = true
     in
     let s_zip_ys_1059 = ys_1032
     in
     let s_zip_xs_1058 = xs_1031
     in
       if xs_1031 <= 0 then
         0
       else
         let xs'_1033 = xs_1031 - 1
         in
           if ys_1032 <= 0 then
             0
           else
             let ys'_1034 = ys_1032 - 1
             in
               1 +
               zip_without_checking_1077 set_flag_zip_1063 s_zip_xs_1058
                 s_zip_ys_1059 xs'_1033 set_flag_zip_1063 s_zip_xs_1058
                 s_zip_ys_1059 ys'_1034

   let rec zip_1030 x_DO_NOT_CARE_1079 x_DO_NOT_CARE_1080 x_DO_NOT_CARE_1081 xs_1031 prev_set_flag_zip_1062 s_prev_zip_xs_1060 s_prev_zip_ys_1061 ys_1032 =
     let u = if prev_set_flag_zip_1062 then
              if ((0 * 1) + (1 * s_prev_zip_xs_1060)) +
                 (0 * s_prev_zip_ys_1061) >
                 ((0 * 1) + (1 * xs_1031)) + (0 * ys_1032) &&
                 ((0 * 1) + (1 * xs_1031)) + (0 * ys_1032) >= 0 then
                ()
              else
                let u_2582 = fail ()
                in
                  bot()
             else
               ()
     in
            zip_without_checking_1077 x_DO_NOT_CARE_1079 x_DO_NOT_CARE_1080
              x_DO_NOT_CARE_1081 xs_1031 prev_set_flag_zip_1062
              s_prev_zip_xs_1060 s_prev_zip_ys_1061 ys_1032

   let main (l1_1037:int(*-:{v:Int | true}*)) (l2_1038:int(*-:{v:Int | true}*)) =
     let set_flag_zip_1063 = false in
     let s_zip_xs_1058 = 0 in
     let s_zip_ys_1059 = 0 in
       zip_1030 set_flag_zip_1063 s_zip_xs_1058 s_zip_ys_1059 l1_1037
         set_flag_zip_1063 s_zip_xs_1058 s_zip_ys_1059 l2_1038
