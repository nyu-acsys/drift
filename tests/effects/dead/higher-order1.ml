(*
 ./drift.exe -prop tests/effects/higher-order1.yml.prp -file tests/effects/higher-order1.ml -domain Polka_st -sen 1 -ev-trans false -trace-len 1 -if-part true
 *)
let caller f =
  (f 1)

let main n =
begin
  caller (fun x -> ev 1; x); ev 2
end
