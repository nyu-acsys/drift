open AbstractDomain
open SemanticDomain
open AbstractTransformer
open Syntax
open Config
open Util
open Parser
open Printer

let x = "x"
let y = "y"
let f = "f"
let g = "g"
let r = "r"
let dec = "dec"
let id = "id"
let loop = "loop"

(* let test_1 = mk_lets [id, mk_lambda x (mk_var x)]
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

let id_test_1 = parse_from_string "let id x = x in id (id 2)"
(* id_test1
((lambda id^0. (id^1 (id^2 2^3)^4)^5)^6 (lambda x^7. x^8)^9)^10
*)

let id_test_2 = parse_from_string "let id x = x in let _ = id 1 in id 2"
(* id_test_2
((lambda id^0. ((lambda _^1. (id^2 2^3)^4)^5 (id^6 1^7)^8)^9)^10
  (lambda x^11. x^12)^13)^14
*)

let id_test_3 = parse_from_string 
"let rec g x n = if n > 0 then g x (n - 1) else x in
  let id x = g (g x 4) 2 in 
  let _ = id 1 in id 4"

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

let op_test_4 = parse_from_string "let f a b = a mod b in f 30 23"

let op_test_5 = parse_from_string "
  let min a b = if a < b then a else b
  let main (m(*-:{v:Int | v > 0}*)) (n(*-:{v:Int | v > 0}*)) = 
  m / (min m n) * n"

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

let rec_test_1 = parse_from_string "
let main (n(*-:{v:Int | v >= 0}*)) =
  let rec foldr rf rn ru = (* foldr *)
	    if rn = 0 then ru
	    else rf rn (foldr rf (rn - 1) ru)
	in
  let gt_100 gk gt = if gk > 5 then 5 else gt
	in
  foldr gt_100 n 0"
(* rec_test_1
*)

let rec_test_2 = parse_from_string "let rec f a = if a <= 1 then 0 else f (a - 1) in f 8"
(* rec_test_2
((lambda f^0. (f^1 8^2)^3)^4
  (mu f^5 a^6. ((a^7 <= 9^8)^9 ? 0^10 : (f^11 (a^12 + 1^13)^14)^15)^16)^17)^18
*)

let rec_test_3 = parse_from_string "let rec f a b = if a <= b then 0 else f (a - 1) b in f 8 1"

let rec_test_4 = parse_from_string "
let rec loop a b = 
  if (a < 3) then 
    loop (a + 1) (b + 2)
  else b
in loop 0 0"
(*
((lambda f^0. ((f^1 0^2)^3 0^4)^5)^6
  (mu f^7 a^8.
     (lambda b^9.
        ((a^10 < 3^11)^12 ? ((f^13 (a^14 + 1^15)^16)^17 (b^18 + 2^19)^20)^21
          : b^22)^23)^24)^25)^26
*)

let rec_test_5 = parse_from_string 
  "let rec loop (a(*-:{v:Int | v > 0}*)) (b(*-:{v:Int | v > 0}*)) = 
    if (a <= 0 || b <= 0) then 
      0
    else if (a = b) then a
    else if (a > b) then
      loop (a - b) b
    else loop a (b - a)"

let rec_test_6 = parse_from_string "
let main (n(*-:{v:Int | v >= 0}*)) =
  let rec reduce rf rn ru = (* foldr *)
	    if rn <= 0 then ru
	    else rf rn (reduce rf (rn - 1) ru)
	in
  let gt_5 gk gt = if gk > 5 then 5 else gt
	in
  reduce gt_5 n 5
  "

let order_test_1 = parse_from_string 
"let id x = x in let succ s = s + 1 in let mult y = y * 2 in
succ (id (mult 2))"

let ary_test_1 = parse_from_string "let f a = Array.length a in let ary = Array.make 3 0 in f ary"
(* ary_test_1
((lambda ary^0. (len^1 ary^2)^3)^4 ((make^5 3^6)^7 0^8)^9)^10
*)

let ary_test_2 = parse_from_string "let ary = Array.make 3 0 in Array.get ary 2"
(* ary_test_2
((lambda ary^0. ((get^1 ary^2)^3 2^4)^5)^6 ((make^7 3^8)^9 0^10)^11)^12
*)

let ary_test_3 = parse_from_string "let ary = Array.make 3 0 in Array.set ary 0 1"
(* ary_test_3
((lambda ary^0. (((set^1 ary^2)^3 2^4)^5 1^6)^7)^8
  ((make^9 3^10)^11 0^12)^13)^14
*)

let ary_test_4 = parse_from_string "let f a1 a2 = Array.length a2 in let ary1 = Array.make 3 0 in let ary2 = Array.make 3 1 in f ary1 ary2"

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

let flow_test = parse_from_string
"
let id x = x in
let x = 1 in
let _ = assert(id 1 = 1) in
assert(id 2 = 2)
"

let list_test_1 = parse_from_string "let f a = List.length a in let lst = List.cons 1 [] in f lst"
(* list_test_1
((lambda f^0.
    ((lambda lst^1. (f^2 lst^3)^4)^5 ((List.cons^6 1^7)^8 []^9)^10)^11)^12
  (lambda a^13. (List.length^14 a^15)^16)^17)^18
*)

let list_test_2 = parse_from_string 
"let lst = List.cons 1 [] in 
let lst' = List.cons 2 lst in
List.hd (List.cons 3 lst')"
(* list_test_2
((lambda lst^0.
    ((lambda lst'^1. (List.hd^2 ((List.cons^3 3^4)^5 lst'^6)^7)^8)^9
      ((List.cons^10 2^11)^12 lst^13)^14)^15)^16
  ((List.cons^17 1^18)^19 []^20)^21)^22
*)

let list_test_3 = parse_from_string "
let lst = List.cons 1 [] in 
let lst' = List.cons 2 lst in
assert(List.length (List.tl lst') = 1)"
(* list_test_3
((lambda lst^0.
    ((lambda lst'^1.
        (((List.length^2 (List.tl^3 lst'^4)^5)^6 = 1^7)^8 ? ()^9 : ()^10)^11)^12
      ((List.cons^13 2^14)^15 lst^16)^17)^18)^19
  ((List.cons^20 1^21)^22 []^23)^24)^25
 *)

let list_test_4 = parse_from_string "
let f l1 l2 = (List.hd l2) - (List.hd l1) in 
let lst1 = List.cons 1 [] in 
let lst2 = List.cons 2 lst1 in 
f lst1 lst2"
(* list_test_4 Should get overappoximate result 0<=v<=1
((lambda f^0.
    ((lambda lst1^1.
        ((lambda lst2^2. ((f^3 lst1^4)^5 lst2^6)^7)^8
          ((List.cons^9 2^10)^11 lst1^12)^13)^14)^15
      ((List.cons^16 1^17)^18 []^19)^20)^21)^22
  (lambda l1^23.
     (lambda l2^24. ((List.hd^25 l2^26)^27 - (List.hd^28 l1^29)^30)^31)^32)^33)^34
 *)

let pair_test_1 = parse_from_string
"1, 2" *)

let unary_minus_test = parse_from_string
"let a = 3 in
let b = -a in
-b"

let tests = [unary_minus_test]
  
let _ =
  try
    Config.parse;
    Printexc.record_backtrace !Config.bt;
    let t = if !Config.parse_file then 
      begin
        pre_vars := VarDefMap.empty;
        thresholdsSet := ThresholdsSetType.empty;
        [parse_from_file !Config.file]
      end
    else tests in
    List.iter (fun e -> 
      let el = e |> simplify |> label in
      if !out_put_level < 2 then
        (print_endline "Executing:";
         print_exp stdout el);
      print_endline "\n";
      print_endline ("Domain specification: " ^ !Config.domain);
      print_endline "\n";
      (* exit 0; *)
      ignore (s el);
      if !out_put_level < 2 then
        print_exp stdout el;
      print_newline ()) t;
    if !Config.debug then print_measures ()
  with
  | Sys_error s | Failure s -> 
      let bs = if !Config.debug then Printexc.get_backtrace () else "" in
      output_string stderr ("Fatal Error: " ^ s ^ "\n" ^ bs); exit 1
  | e ->
      let bs = if !Config.debug then Printexc.get_backtrace () else "" in
      output_string stderr ("Fatal Error: " ^ Printexc.to_string e ^ "\n" ^ bs); exit 1
