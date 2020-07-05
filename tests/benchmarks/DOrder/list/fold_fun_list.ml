

let rec make_list n =
  if n <= 0
  then []
  else (fun m -> n + m) :: make_list (n - 1)

let rec fold_right (f:(int->int)->(int->int)->int->int) (xs:(int->int) list) (init:int->int) =
  match xs with
      [] -> init
    | x::xs' -> f x (fold_right f xs' init)

let compose (f:int->int) (g:int->int) (d:int) = f (g d)

let id (c:int) = c

let main (n:int(*-:{v:Int | true}*)) =
  (* let xs = make_list n in
  match xs with
  | [] -> 0
  | x :: xs' -> x 2 *)
  if n > 0 then
    let xs = make_list n in
    let f = fold_right compose xs id in
    assert (f 0 >= 0)
  else ()

(* xs |-> { cur_v: 'a List (l0, e0) | len:  [|-l0+n=0; prefn-1>=0; l0>=0|] , item:
   z4: (_|_ -> _|_) } *)

(* ((lambda make_list^0.
    ((lambda fold_right^1.
        ((lambda compose^2.
            ((lambda id^3.
                ((lambda main^4. (main^5 prefn^6)^7)^8
                  (lambda n^9.
                     ((n^10 > 0^11)^12 ?
                       ((lambda xs^13.
                           ((lambda f^14.
                               (((f^15 0^16)^17 >= 0^18)^19 ? ()^20 :
                                 ()^21)^22)^23
                             (((fold_right^24 compose^25)^26 xs^27)^28
                               id^29)^30)^31)^32
                         (make_list^33 n^34)^35)^36
                       : ()^37)^38)^39)^40)^41
              (lambda c^42. c^43)^44)^45)^46
          (lambda f^47.
             (lambda g^48. (lambda d^49. (f^50 (g^51 d^52)^53)^54)^55)^56)^57)^58)^59
      (mu fold_right^60 f^61.
         (lambda xs^62.
            (lambda init^63.
               (match xs^64 with []^65 -> init^66 | (x^67 :: xs'^68)^69 ->
                 ((f^70 x^71)^72
                   (((fold_right^73 f^74)^75 xs'^76)^77 init^78)^79)^80)^81)^82)^83)^84)^85)^86
  (mu make_list^87 n^88.
     ((n^89 <= 0^90)^91 ? []^92 :
       ((lambda m^93. (n^94 + m^95)^96)^97 ::
          (make_list^98 (n^99 - 1^100)^101)^102)^103)^104)^105)^106 *)