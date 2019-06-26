open AbstractDomain
open SemanticDomain
open AbstractTransformer
open Syntax
open Util

let label e =
    let rec l k = function
      | Const (c, _) -> Const (c, k), k + 1
      | Var (x, _) -> Var (x, k), k + 1
      | App (e1, e2, _) ->
          let e1', k1 = l k e1 in
          let e2', k2 = l k1 e2 in
          App (e1', e2', k2), k2 + 1
      | Rec (fopt, x, _, e1, _) ->
          let fopt', k1 =
            fopt |>
            Opt.map (function (f, _) -> Some (f, k), k + 1) |>
            Opt.get_or_else (None, k)
          in
          let e1', k2 = l (k1 + 1) e1 in
        Rec (fopt', x, k1, e1', k2), k2 + 1
      | Ite (e0, e1, e2, _) ->
          let e0', k0 = l k e0 in
          let e1', k1 = l k0 e1 in
          let e2', k2 = l k1 e2 in
          Ite (e0', e1', e2', k2), k2 + 1
      | BinOp (bop, e1, e2, _) ->
        let e1', k1 = l k e1 in
        let e2', k2 = l k1 e2 in
        BinOp (bop, e1, e2, k2), k2 + 1
    in
    l 0 e |> fst

let mk_int k = Const (Integer k, 0)
let mk_bool b = Const (Boolean b, 0)
let mk_var x = Var (x, 0)
let mk_app e1 e2 = App (e1, e2, 0)
let mk_lambda x e = Rec (None, x, 0, e, 0)
let mk_rec f x e = Rec (Some (f, 0), x, 0, e, 0)
let mk_ite e0 e1 e2 = Ite (e0, e1, e2, 0)

let mk_let x def e = mk_app (mk_lambda x  e) def
let mk_lets defs e =
  List.fold_right (fun (x, def) e -> mk_let x def e) defs e

let x = "x"
let y = "y"
let id = "id"

let test_1 = mk_lets [id, mk_lambda x (mk_var x)]
(mk_app
   (mk_app (mk_var id) (mk_var id))
   (mk_int 1))

let fun_test = 
  (mk_app (mk_lambda x (mk_app (mk_var x) (mk_int 1))) (mk_lambda y (mk_var y)))

let tests = [fun_test]

let _ = List.iter (fun e ->
  let line = input_line stdin in
  Config.parse_options line;
  let el = label e in
  print_endline "Executing:";
  print_exp stdout el;
  print_endline "\n";
  print_endline ("Domain specification: " ^ !Config.domain);
  print_endline "\n";
  Printexc.record_backtrace !Config.bt;
  ignore (s el);
  print_newline ()) tests
