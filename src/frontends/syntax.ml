
open Util

(** {1} Type definitions for representing syntax *)

(** Variable names *)

type var = string
type pos = int

let fail pos msg = failwith (Printf.sprintf "Error:%d: %s" pos msg)

(** Binary infix operators *)
type binop =
  | Plus  (* + *)
  | Ge    (* >= *)
  | Mult  (* * *)
  | Div   (* / *)
  | Mod   (* mod *)
  | Minus (* - *)
  | Eq    (* = *)
  | Ne    (* <> *)
  | Lt    (* < *)
  | Gt    (* > *)
  | Le    (* <= *)

type value =
  | Integer of int
  | Boolean of bool

(** Terms *)
type term =
  | Const of value * pos               (* i (int constant) *)
  | Var of var * pos                   (* x (variable) *)
  | App of term * term * pos           (* t1 t2 (function application) *)
  | BinOp of binop * term * term * pos (* t1 bop t2 (binary infix operator) *)
  | Ite of term * term * term * pos    (* if t1 then t2 else t3 (conditional) *)
  | Rec of (var * pos) option * var * pos * term * pos (*lambda and recursive function*)

let loc = function
  | Const (_, l)
  | Var (_, l)
  | App (_, _, l)
  | BinOp (_, _, _, l)
  | Ite (_, _, _, l)
  | Rec (_, _, _, _, l) -> l

let string_of_op = function
  | Plus  (* + *) -> "+"
  | Ge    (* >= *) -> ">="
  | Mult  (* * *) -> "*"
  | Div   (* / *) -> "/"
  | Mod   (* mod *) -> "%"
  | Minus (* - *) -> "-"
  | Eq    (* = *) -> "="
  | Ne    (* <> *) -> "!="
  | Lt    (* < *) -> "<"
  | Gt    (* > *) -> ">"
  | Le    (* <= *) -> "<="

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
        BinOp (bop, e1', e2', k2), k2 + 1
    in
    l 0 e |> fst

let mk_int k = Const (Integer k, 0)
let mk_bool b = Const (Boolean b, 0)
let mk_var x = Var (x, 0)
let mk_app e1 e2 = App (e1, e2, 0)
let mk_lambda x e = Rec (None, x, 0, e, 0)
let mk_lambdas xs e = List.fold_right (fun x e -> mk_lambda x e) xs e
let mk_rec f x e = Rec (Some (f, 0), x, 0, e, 0)
let mk_ite e0 e1 e2 = Ite (e0, e1, e2, 0)
let mk_op op e1 e2 = BinOp (op, e1, e2, 0)

let mk_let x def e = mk_app (mk_lambda x e) def
let mk_lets defs e =
  List.fold_right (fun (x, def) e -> mk_let x def e) defs e

let mk_let_rec x def e = mk_app (mk_rec x x e) def