
let rec list_rec k lst (*List (l0, e0) | len: l0 = 1 , item : e0 = 1*) = 
	match lst (*List (l0, e0) | len: l0 = 1 , item : e0 = 1*) with
	 [] (*List (l0, e0) | len: l0 = 0 , item : _|_*) 
	 	-> [] (*List (l1, e1) | len: l1 = 0 , item : _|_*)
	| hd :: tl (*List (l2, e2) | len: l2 = 0 , item : _|_*) -> (k :: 
		(list_rec (k + 1) tl) (*List (l2, e2) | len: l2 = 0 , item : _|_*))
		(*List (l1, e1) | len: l1 = 1 , item : e1 = 1*)

(*
Rec node stores
prop (*List (l2, e2) | len: l2 = 0 , item : _|_*)
with
	(*List (l0, e0) | len: l0 = 1 , item : e0 = 1*)
*)

let rec make_list n =
  if n = 0
  then [] (* 'a list *)
  else n :: make_list (n - 1)

let main (n:int(*-:{v:Int | true }*)) =
  if n > 0 then
  	let xs = make_list n in
  	let ys = list_rec 0 xs in
  	assert(List.length ys = List.length xs)
  else ()

(*
((lambda list_rec^0.
    ((lambda make_list^1.
        ((lambda main^2. (main^3 prefn^4)^5)^6
          (lambda n^7.
             ((lambda xs^8. ((list_rec^9 0^10)^11 xs^12)^13)^14
               (make_list^15 n^16)^17)^18)^19)^20)^21
      (mu make_list^22 n^23.
         ((n^24 = 0^25)^26 ? []^27 :
           (n^28 :: (make_list^29 (n^30 - 1^31)^32)^33)^34)^35)^36)^37)^38
  (mu list_rec^39 k^40.
     (lambda lst^41.
        (match lst^42 with []^43 -> []^44 |
          (hd^45 :: tl^46)^47 ->
            (k^48 :: ((list_rec^49 (k^50 + 1^51)^52)^53 tl^54)^55)^56)^57)^58)^59)^60
*)