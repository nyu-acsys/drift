/*
Generated by
MoCHi: Model Checker for Higher-Order Programs
  Build: 10b9033
  Z3 library: Z3 4.15.2.0
  Z3 binary: Z3 version 4.15.2 - 64 bit
  HorSat2 version: 0.95
  HoIce version: 1.10.0
  OCaml version: 4.14.1
  Command: mochi.exe -trans HFLz ../../tests/effects/tr_tuple_mochi/sum-appendix.ml
*/

%HES
MAIN_16  =v
  EV_STEP_ASST0 0 (2 + 0)
   (\u_159.
     EV_STEP_ASST0 0 (3 + (2 + 0))
      (\u1_170.
        EV_STEP_ASST0 0 (1 + (3 + (2 + 0)))
         (\u2_187.
           EV_STEP_ASST0 0 (4 + (1 + (3 + (2 + 0))))
            (\u3_210.
              EV_STEP_ASST0 0 ((10 + 20) + (4 + (1 + (3 + (2 + 0)))))
               (\u4_137.(10 + 20) + (4 + (1 + (3 + (2 + 0)))) = 40))))).
EV_STEP_ASST0 cfg10 cfg11 k =v (cfg11 < 0 \/ k true) /\ cfg11 >= 0.
Forall p      =v ∀n. p n.
