
open AbstractDomain
open AbstractTransformer
open Syntax

let mk_int k = Const (Integer k, 0)
let mk_bool b = Const (Boolean b, 0)
let mk_var x = Var (x, 0)
let mk_app e1 e2 = App (e1, e2, 0)
let mk_lambda x e = Rec (None, x, 0, e, 0)
let mk_rec f x e = Rec (Some (f, 0), x, 0, e, 0)
let mk_ite e0 e1 e2 = Ite (e0, e1, e2, 0)

let x = "x"
let y = "y"
let id = "id"

let test_1 = let id  = mk_lambda x (mk_var x) in
    let _ = mk_app id (mk_int 1) in
    mk_app id (mk_int 2)

let fun_test = 
  (mk_app (mk_lambda x (mk_app (mk_var x) (mk_int 1))) (mk_lambda y (mk_var y)))

let tests = [fun_test]

let _ = List.iter (fun e ->
  let el = label e in
  print_endline "Executing:";
  print_exp stdout el;
  print_endline "\n";
  ignore (s el);
  print_newline ()) tests
