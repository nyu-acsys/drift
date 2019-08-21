open AbstractDomain
open SemanticDomain
open AbstractTransformer
open Syntax
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

let id_test1 =
  mk_lets
    [id, mk_lambda x (mk_var x);
      x, mk_app (mk_var id) (mk_int 1);
      y, mk_app (mk_var id) (mk_int 2)]
    (mk_var y)

let prop_test = parse_from_string "let id a = a in (id id) 1)"

let fun_test = 
  (mk_app (mk_lambda x (mk_app (mk_var x) (mk_int 1))) (mk_lambda y (mk_var y)))

let op_test = let dec_def = mk_lambda y (mk_op Plus (mk_int (-1)) (mk_var y)) in
  (mk_app dec_def (mk_int 1))

let op_test_2 = let dec_def = mk_lambda x (mk_lambda y (mk_op Plus (mk_var x) (mk_var y))) in
  (mk_app (mk_app dec_def (mk_int 3)) (mk_int 4))

let op_test_3 = parse_from_string "let f a b = a + b in f (f 1 2) 3"
(*
((lambda f^0. ((f^1 ((f^2 1^3)^4 2^5)^6)^7 3^8)^9)^10
  (lambda a^11. (lambda b^12. (a^13 + b^14)^15)^16)^17)^18
*)

let if_test = let def_if = (mk_lambda x (mk_lambda y (mk_ite (mk_op Gt (mk_var x) (mk_var y)) (mk_var x) (mk_var y)))) in
  (mk_app (mk_app def_if (mk_int 1)) (mk_int 2))

let if_test_2 = parse_from_string "let f a = if a > 1 then a else 1 in f 2"

let rec_test = parse_from_string "let rec f a = if a > 9 then 10 else f (a + 1) in f 9"
(*
((lambda f^0. (f^1 9^2)^3)^4
  (mu f^5 a^6. ((a^7 > 9^8)^9 ? 10^10 : (f^11 (a^12 + 1^13)^14)^15)^16)^17)^18
*)

let tests = [rec_test]

let _ = List.iter (fun e ->
  Config.parse;
  Printexc.record_backtrace !Config.bt;
  let el = label e in
  print_endline "Executing:";
  print_exp stdout el;
  print_endline "\n";
  print_endline ("Domain specification: " ^ !Config.domain);
  print_endline "\n";
  ignore (s el);
  print_newline ()) tests
