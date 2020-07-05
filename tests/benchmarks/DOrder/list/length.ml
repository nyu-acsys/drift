let rec length (xs:int list) =
  match xs with
      [] -> 0 (*l=0 v=0*)
    | x::xs' -> 1 + length xs' (*n>=l>=1 v=1*)

let rec make_list n =
  if n = 0
  then [] (* 'a list *)
  else n :: make_list (n - 1)

let main (n:int(*-:{v:Int | true }*)) =
  if n > 0 then
	  let xs = make_list n in
	  assert (length xs = n)
  else ()

(*
((lambda length^0.
    ((lambda make_list^1.
        ((lambda main^2. (main^3 prefn^4)^5)^6
          (lambda n^7.
             ((n^8 > 0^9)^10 ?
               ((lambda xs^11.
                   (((length^12 xs^13)^14 = n^15)^16 ? ()^17 : ()^18)^19)^20
                 (make_list^21 n^22)^23)^24
               : ()^25)^26)^27)^28)^29
      (mu make_list^30 n^31.
         ((n^32 = 0^33)^34 ? []^35 :
           (n^36 :: (make_list^37 (n^38 - 1^39)^40)^41)^42)^43)^44)^45)^46
  (mu length^47 xs^48.
     (match xs^49 with []^50 -> 0^51 | (x^52 :: xs'^53)^54 ->
       (1^55 + (length^56 xs'^57)^58)^59)^60)^61)^62
*)