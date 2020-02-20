



let rec accumulate af an au = (* foldl *)
    if an = 0 then au
	else accumulate af (an - 1) (af an au)
in

let _ = accumulate (fun k t -> if t > 100 then 100 else t) 3 0 in
accumulate (fun k t -> t && k <> 0) 3 true


(* ((lambda accumulate^0.
    ((lambda _^1.
        (((accumulate^2
            (lambda k^3. (lambda t^4. (t^5 && (k^6 != 0^7)^8)^9)^10)^11)^12
           3^13)^14
          true^15)^16)^17
      (((accumulate^18
          (lambda k^19.
             (lambda t^20. ((t^21 > 100^22)^23 ? 100^24 : t^25)^26)^27)^28)^29
         3^30)^31
        0^32)^33)^34)^35
  (mu accumulate^36 af^37.
     (lambda an^38.
        (lambda au^39.
           ((an^40 = 0^41)^42 ? au^43 :
             (((accumulate^44 af^45)^46 (an^47 - 1^48)^49)^50
               ((af^51 an^52)^53 au^54)^55)^56)^57)^58)^59)^60)^61 *)