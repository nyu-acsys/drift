let rec loop x i =
  if i < 0 then
    x
  else if x < 1 then (* x <= 0*)
    loop (x - 1) (i - 1)
  else if x > 2 then (* x >= 3*)
    loop x (i - 1)
  else (* 1 2 *)
    loop (3 - x) (i - 1)
in 
loop 2 3

(*
((lambda loop^0. ((loop^1 2^2)^3 3^4)^5)^6
  (mu loop^7 x^8.
     (lambda i^9.
        ((i^10 < 0^11)^12 ? x^13 :
          ((x^14 < 1^15)^16 ?
            ((loop^17 (x^18 - 1^19)^20)^21 (i^22 - 1^23)^24)^25 :
            ((x^26 > 2^27)^28 ? ((loop^29 x^30)^31 (i^32 - 1^33)^34)^35 :
              ((loop^36 (3^37 - x^38)^39)^40 (i^41 - 1^42)^43)^44)^45)^46)^47)^48)^49)^50
Why top when x = [1,2]?
During widening, we lost one bound, either right or left.
However, the conditional branch will not just calculated the last else branch.
It might go the second or third else and join the input bind of x be top.
*)