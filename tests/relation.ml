
let rec recurs_n rn ri =
	let c = 0 in
	let d = 9 in
	if rn <= 0 then ri
	else 
    (let r = 5 ;
    recurs_n (rn - 1) (ri + 1)
    in r)

let main (n(*-:{v:Int | v > 0}*)) = 
    recurs_n n 0



(* ((lambda recurs_n^0.
    ((lambda main^1. (main^2 prefn^3)^4)^5
      (lambda n^6. ((recurs_n^7 n^8)^9 0^10)^11)^12)^13)^14
  (mu recurs_n^15 rn^16.
     (lambda ri^17.
        ((lambda c^18.
            ((rn^19 <= 0^20)^21 ? ri^22 :
              ((recurs_n^23 (rn^24 - 1^25)^26)^27 (ri^28 + 1^29)^30)^31)^32)^33
          0^34)^35)^36)^37)^38 *)
