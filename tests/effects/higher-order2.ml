(*
 ./drift.exe -prop tests/effects/higher-order.yml.prp -file tests/effects/higher-order.ml -domain Polka_st -sen 1 -ev-trans false -trace-len 1 -if-part true
 *)
let caller f =
  ev (f 1)

let main n =
begin
  caller (fun x -> x); ev 2
end
