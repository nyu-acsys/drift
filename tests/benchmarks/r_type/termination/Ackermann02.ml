
let rec bot _ = bot ()
let fail _ = assert false

let rec ack_1030 ack_without_checking_1087 x_DO_NOT_CARE_1089 x_DO_NOT_CARE_1090 x_DO_NOT_CARE_1091 m_1031 prev_set_flag_ack_1074 s_prev_ack_m_1072 s_prev_ack_n_1073 n_1032 =
  let u =
     if prev_set_flag_ack_1074 then
              if ((0 * 1) + (0 * s_prev_ack_m_1072)) + (1 * s_prev_ack_n_1073)
                 > ((0 * 1) + (0 * m_1031)) + (1 * n_1032) &&
                 ((0 * 1) + (0 * m_1031)) + (1 * n_1032) >= 0 then
                ()
              else
                let u_3767 = fail ()
                in
                  bot()
     else
       ()
         in
            ack_without_checking_1087 x_DO_NOT_CARE_1089 x_DO_NOT_CARE_1090
              x_DO_NOT_CARE_1091 m_1031 prev_set_flag_ack_1074
              s_prev_ack_m_1072 s_prev_ack_n_1073 n_1032

   let rec ack_without_checking_1087 x_DO_NOT_CARE_1093 x_DO_NOT_CARE_1094 x_DO_NOT_CARE_1095 m_1031 set_flag_ack_1075 s_ack_m_1070 s_ack_n_1071 n_1032 =
     let set_flag_ack_1075 = true
     in
     let s_ack_n_1071 = n_1032
     in
     let s_ack_m_1070 = m_1031
     in
       if m_1031 = 0 then
         n_1032 + 1
       else
         if n_1032 = 0 then
           ack_without_checking_1087 set_flag_ack_1075 s_ack_m_1070
             s_ack_n_1071 (m_1031 - 1) set_flag_ack_1075 s_ack_m_1070
             s_ack_n_1071 1
         else
           ack_without_checking_1087 set_flag_ack_1075 s_ack_m_1070
             s_ack_n_1071 (m_1031 - 1) set_flag_ack_1075 s_ack_m_1070
             s_ack_n_1071
             (ack_1030 ack_without_checking_1087 set_flag_ack_1075 s_ack_m_1070 s_ack_n_1071 m_1031
               set_flag_ack_1075 s_ack_m_1070 s_ack_n_1071 (n_1032 - 1))

   let main_1033 set_flag_ack_1075 s_ack_m_1070 s_ack_n_1071 u_1034 m_1035 n_1036 =
       if n_1036 > 0 && m_1035 > 0 then
         ack_without_checking_1087 set_flag_ack_1075 s_ack_m_1070 s_ack_n_1071
           m_1035 set_flag_ack_1075 s_ack_m_1070 s_ack_n_1071 n_1036
       else
         0

   let main =
     main_1033 false 0 0 ()
