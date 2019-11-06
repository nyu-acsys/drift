let rec fib n =
  if n<2 then 1 else
    fib (n - 1) + fib (n - 2) in

fib 50

(*
(
(lambda fib^0. (fib^1 50^2)^3)^4
  (mu fib^5 n^6.
     (
     (n^7 < 2^8)^9 ? 1^10 :
       (
       (fib^11 (n^12 - 1^13)^14)^15 + (fib^16 (n^17 - 2^18)^19)^20
       )^21
     )^22
   )^23
)^24
*)
