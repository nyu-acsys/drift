(*
Assertion error
*)

let main (l1_1037(*-:{v:Int | true}*)) (l2_1038(*-:{v:Int | true}*)) =
  let rec bot bx = bot () in
  let fail fx = assert (false) in

  let rec zip_without_checking_1077 x_DO_NOT_CARE_1083 x_DO_NOT_CARE_1084 x_DO_NOT_CARE_1085 xs_10311 set_flag_zip_1063 s_zip_xs_1058 s_zip_ys_1059 ys_10322 =
    let set_flag_zip_1063_r = true in
    let s_zip_ys_1059_r = ys_10322 in
    let s_zip_xs_1058_r = xs_10311 in
    if xs_10311 <= 0 then
      0
    else
      let xsp_1033 = xs_10311 - 1 in
        if ys_10322 <= 0 then
          0
        else
          let ysp_1034 = ys_10322 - 1
          in
            1 +
            zip_without_checking_1077 set_flag_zip_1063_r s_zip_xs_1058_r
              s_zip_ys_1059_r xsp_1033 set_flag_zip_1063_r s_zip_xs_1058_r
              s_zip_ys_1059_r ysp_1034
  in

  let rec zip_1030 x_DO_NOT_CARE_1079 x_DO_NOT_CARE_1080 x_DO_NOT_CARE_1081 xs_1031 prev_set_flag_zip_1062 s_prev_zip_xs_1060 s_prev_zip_ys_1061 ys_1032 =
    let u =if prev_set_flag_zip_1062 then
     let u_1166 = fail () in
      bot()
      else () in
    zip_without_checking_1077 x_DO_NOT_CARE_1079 x_DO_NOT_CARE_1080
      x_DO_NOT_CARE_1081 xs_1031 prev_set_flag_zip_1062
      s_prev_zip_xs_1060 s_prev_zip_ys_1061 ys_1032
  in

  let set_flag_zip_1063m = false in
  let s_zip_xs_1058m = 0 in
  let s_zip_ys_1059m = 0 in
  let res = zip_1030 set_flag_zip_1063m s_zip_xs_1058m s_zip_ys_1059m l1_1037
      set_flag_zip_1063m s_zip_xs_1058m s_zip_ys_1059m l2_1038 in
  assert(res < 0)

(* in main 103 1034  *)

