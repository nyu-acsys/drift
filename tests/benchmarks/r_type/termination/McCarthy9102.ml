
let rec bot _ = bot ()
let fail _ = assert false

   let rec mc91_1030 mc91_without_checking_1058 prev_set_flag_mc91_1049 s_prev_mc91_n_1048 n_1031 =
     let u = if prev_set_flag_mc91_1049 then
              if (111 * 1) + (-s_prev_mc91_n_1048) > (111 * 1) + (-n_1031) &&
                 (111 * 1) + (-n_1031) >= 0 then
                ()
              else
                let u_2195 = fail ()
                in
                  bot()
     else () in
            mc91_without_checking_1058 prev_set_flag_mc91_1049
              s_prev_mc91_n_1048 n_1031


   let rec mc91_without_checking_1058 set_flag_mc91_1050 s_mc91_n_1047 n_1031 =
     let set_flag_mc91_1050 = true
     in
     let s_mc91_n_1047 = n_1031
     in
       if n_1031 > 100 then
         n_1031 - 10
       else
         mc91_without_checking_1058 set_flag_mc91_1050 s_mc91_n_1047
           (mc91_1030 mc91_without_checking_1058 set_flag_mc91_1050 s_mc91_n_1047 (n_1031 + 11))

   let main (r:int(*-:{v:Int | true}*)) =
     let set_flag_mc91_1050 = false in
     let s_mc91_n_1047 = 0 in
     mc91_without_checking_1058 set_flag_mc91_1050 s_mc91_n_1047 r
