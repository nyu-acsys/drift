open AbstractDomain
open SemanticDomain
open AbstractTransformer
open Syntax
open Config
open Util
open Parser

let x = "x"
let y = "y"
let f = "f"
let g = "g"
let r = "r"
let dec = "dec"
let id = "id"
let loop = "loop"

let test_1 = mk_lets [id, mk_lambda x (mk_var x)]
(mk_app
   (mk_app (mk_var id) (mk_var id))
   (mk_int 1))

let overview_test =
  let dec_def = mk_lambda y (mk_op Plus (mk_int (-1)) (mk_var y)) in
  let f_def =
    mk_lambda x
      (mk_lambda g
          (mk_ite
            (mk_op Ge (mk_int 0) (mk_var x))
            (mk_app (mk_var g) (mk_var x))
            (mk_var x)))
  in
  mk_lets
    [dec, dec_def;
      f, f_def]
    (mk_app
        (mk_app
          (mk_var f)
          (mk_app
              (mk_app (mk_var f) (mk_int 1))
              (mk_var dec)))
        (mk_var dec))

let id_test_1 = parse_from_string "let id x = x in (id id) 2"
(* id_test1
((lambda id^0. ((id^1 id^2)^3 2^4)^5)^6 (lambda x^7. x^8)^9)^10
*)

let id_test_2 = parse_from_string "let id x = x in let _ = id 1 in id 2"
(* id_test_2
((lambda id^0. ((lambda _^1. (id^2 2^3)^4)^5 (id^6 1^7)^8)^9)^10
  (lambda x^11. x^12)^13)^14
*)

let id_test_3 = parse_from_string "let id x = x in let _ = id 1 in id true"
(* id_test_2
((lambda id^0. ((lambda _^1. (id^2 2^3)^4)^5 (id^6 1^7)^8)^9)^10
  (lambda x^11. x^12)^13)^14
*)

let fun_test = 
  (mk_app (mk_lambda x (mk_app (mk_var x) (mk_int 1))) (mk_lambda y (mk_var y)))

let op_test = let dec_def = mk_lambda y (mk_op Plus (mk_int (-1)) (mk_var y)) in
  (mk_app dec_def (mk_int 1))

let op_test_2 = let dec_def = mk_lambda x (mk_lambda y (mk_op Plus (mk_var x) (mk_var y))) in
  (mk_app (mk_app dec_def (mk_int 3)) (mk_int 4))
(* op_test_2
(((lambda x^0. (lambda y^1. (x^2 + y^3)^4)^5)^6 3^7)^8 4^9)^10
*)

let op_test_3 = parse_from_string "let f a b = a + b in f (f 1 2) 3"
(* op_test_3
((lambda f^0. ((f^1 ((f^2 1^3)^4 2^5)^6)^7 3^8)^9)^10
  (lambda a^11. (lambda b^12. (a^13 + b^14)^15)^16)^17)^18
*)

let op_test_4 = parse_from_string "let f a b = a + b * b in f 3 2"

let if_test_1 = let def_if = (mk_lambda x (mk_lambda y (mk_ite (mk_op Gt (mk_var x) (mk_var y)) (mk_var x) (mk_var y)))) in
  (mk_app (mk_app def_if (mk_int 1)) (mk_int 2))
(* if_test_1
(((lambda x^0. (lambda y^1. ((x^2 > y^3)^4 ? x^5 : y^6)^7)^8)^9 1^10)^11
  2^12)^13
*)

let if_test_2 = parse_from_string "let f a = if a > 2 then a else 1 in f 2"

let if_test_3 = parse_from_string "let f a b = if a > 5 then if b < 15 then 0 else 1 else 2 in f 6 10"
(*
((lambda f^0. ((f^1 6^2)^3 10^4)^5)^6
  (lambda a^7.
     (lambda b^8.
        ((a^9 > 5^10)^11 ? ((b^12 < 15^13)^14 ? 0^15 : 1^16)^17 : 2^18)^19)^20)^21)^22
*)

let rec_test_1 = parse_from_string "let rec f a = if a + 1 > 9 then 10 else f (a + 1) in f 1"
(* rec_test_1
((lambda f^0. (f^1 8^2)^3)^4
  (mu f^5 a^6.
     (((a^7 + 1^8)^9 > 9^10)^11 ? 
        10^12 : 
        (f^13 (a^14 + 1^15)^16)^17)^18)^19)^20
*)

let rec_test_2 = parse_from_string "let rec f a = if a <= 1 then 0 else f (a - 1) in f 8"
(* rec_test_2
((lambda f^0. (f^1 8^2)^3)^4
  (mu f^5 a^6. ((a^7 <= 9^8)^9 ? 0^10 : (f^11 (a^12 + 1^13)^14)^15)^16)^17)^18
*)

let rec_test_3 = parse_from_string "let rec f a b = if a <= b then 0 else f (a - 1) b in f 8 1"

let rec_test_4 = parse_from_string 
  "let rec f a b = if a >= 10 then b else f (a + 1) b in 
    let _ = f 8 1 in f 8 true"
(*
((lambda f^0.
    ((lambda _^1. ((f^2 8^3)^4 true^5)^6)^7 ((f^8 8^9)^10 1^11)^12)^13)^14
  (mu f^15 a^16.
     (lambda b^17.
        ((a^18 >= 10^19)^20 ? b^21 : ((f^22 (a^23 + 1^24)^25)^26 b^27)^28)^29)^30)^31)^32
*)

let ary_test_1 = parse_from_string "let f a = len a in let ary = make 3 0 in f ary"
(* ary_test_1
((lambda ary^0. (len^1 ary^2)^3)^4 ((make^5 3^6)^7 0^8)^9)^10
*)

let ary_test_2 = parse_from_string "let ary = make 3 0 in get ary 2"
(* ary_test_2
((lambda ary^0. ((get^1 ary^2)^3 2^4)^5)^6 ((make^7 3^8)^9 0^10)^11)^12
*)

let ary_test_3 = parse_from_string "let ary = make 3 0 in set ary 0 1"
(* ary_test_3
((lambda ary^0. (((set^1 ary^2)^3 2^4)^5 1^6)^7)^8
  ((make^9 3^10)^11 0^12)^13)^14
*)

let ary_test_4 = parse_from_string "let f a1 a2 = len a2 in let ary1 = make 3 0 in let ary2 = make 3 1 in f ary1 ary2"

let assert_test = parse_from_string "let a = assert(1 < 2) in a"
(*
((lambda a^0. a^1)^2 ((1^3 < 2^4)^5 ? true^6 : false^7)^8)^9
*)

let scoping_test = parse_from_string "let a = 1 in let b = a + 1 in let c = a + b + 1 in c"

let bool_test_1 = parse_from_string "true && false || true"
(* bool_test_1
((true^0 && false^1)^2 || true^3)^4
*)

let bool_test_2 = parse_from_string "1 < 2 || 3 > 4 && 6 <> 6"
(* bool_test_2
(((1^0 > 2^1)^2 && (3^3 < 4^4)^5)^6 || (6^7 = 6^8)^9)^10
*)

let bool_test_3 = parse_from_string "let rec f a = if a > 1 && a < 10 then f (a + 1) else a in f 4"
(* bool_test_3
((lambda f^0. (f^1 4^2)^3)^4
  (mu f^5 a^6.
     (((a^7 > 1^8)^9 && (a^10 < 10^11)^12)^13 ? (f^14 (a^15 + 1^16)^17)^18 :
       a^19)^20)^21)^22
*)

let seq_test_1 = parse_from_string "let x = if true then 1 else 4 ; 3 in x"
(* seq_test_1
((lambda _^0. 3^1)^2 (1^3 + 2^4)^5)^6
*)

let seq_test_2 = parse_from_string "assert(3 + 4 > 7); assert(3 + 4 < 10)"
(* seq_test_2
((lambda _^0. (((3^1 + 4^2)^3 < 10^4)^5 ? true^6 : false^7)^8)^9
  (((3^10 + 4^11)^12 > 7^13)^14 ? true^15 : false^16)^17)^18
*)

let seq_test_3 = parse_from_string "let a = 1 in a; (fun x -> 6) 9;a"
(* seq_test_3
((lambda a^0. ((lambda _^1. 6^2)^3 3^4)^5)^6 1^7)^8
*)

let seq_test_4 = parse_from_string "if false then 2 else let x = 4 in 3; x"

let typedel_test_1 = parse_from_string "let id (x:int) (z:bool) = if z then x else 2 in let kj (k:int) = k in id 3"

let redef_test_1 = parse_from_string "let f a b = if a < 1 then let a = 2 in a else b in f 0 3"

let scope_test = parse_from_string "let rec f x y = let a = x + 1 in let b = y - 1 in if a > 3 then b else f a b in 
  let w = 1 in
  f w 2"
(*
((lambda f^0. ((lambda w^1. ((f^2 w^3)^4 2^5)^6)^7 1^8)^9)^10
  (mu f^11 x^12.
     (lambda y^13.
        ((lambda a^14.
            ((lambda b^15.
                ((a^16 > 3^17)^18 ? b^19 : ((f^20 a^21)^22 b^23)^24)^25)^26
              (y^27 - 1^28)^29)^30)^31
          (x^32 + 1^33)^34)^35)^36)^37)^38
*)

let normal_test = parse_from_string "let g x = 1 in let f y = 2 in let k x y = 3 in k (f 1) (g 2)"

let high_mult_rec_test_1 = parse_from_string 
  "let k kx = kx in let id ix iy = ix iy in
  let fb xb yb = let zb = yb - 1 in xb zb in
  fb (id k) 2"

let high_mult_rec_test_2 = parse_from_string 
  "let rec id x y = id x y in let f z = z in f id 10 20"

let high_mult_call_test_1 = parse_from_string
  "let h g = g 5 in let g1 x = 1 in let g2 x = x in (h g1) / (h g2)"

let tests = [rec_test_4]

let _ =
  Config.parse;
  Printexc.record_backtrace !Config.bt;
  let t = if !Config.parse_file then [parse_from_file !Config.file]
  else tests in
  List.iter (fun e -> 
  let el = label e in
  if not !integrat_test then
    (print_endline "Executing:";
    print_exp stdout el);
  print_endline "\n";
  print_endline ("Domain specification: " ^ !Config.domain);
  print_endline "\n";
  ignore (s el);
  if not !integrat_test then
    print_exp stdout el;
  print_newline ()) t
