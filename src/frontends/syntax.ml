
open Util

(** {1} Type definitions for representing syntax *)
type pos = { pl: int; pc: int }
(** Variable names *)

type var = string
type loc = string

let fail pos msg =
  failwith (Printf.sprintf "Error: line %d col %d: %s" pos.pl pos.pc msg)

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
  | And   (* && *)
  | Or    (* || *)

type value =
  | Integer of int
  | Boolean of bool

type varmap = var * value

module VarDefMap = Map.Make(struct
  type t = var
  let compare = compare
end)

let fun_name = "temp"

let top_var: varmap list VarDefMap.t ref = ref VarDefMap.empty

(** Terms *)
type term =
  | Void of loc                        (* unit (unit) *)
  | Const of value * loc               (* i (int constant) *)
  | Var of var * loc                   (* x (variable) *)
  | App of term * term * loc           (* t1 t2 (function application) *)
  | BinOp of binop * term * term * loc (* t1 bop t2 (binary infix operator) *)
  | Ite of term * term * term * loc    (* if t1 then t2 else t3 (conditional) *)
  | Rec of (var * loc) option * var * loc * term * loc (*lambda and recursive function*)

let loc = function
  | Void (l)
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
  | And   (* && *) -> "&&"
  | Or    (* || *) -> "||"

let cond_op = function
  | Plus | Mult | Div | Mod | Minus | And | Or -> false
  | Ge | Eq | Ne | Lt | Gt | Le -> true

let bool_op = function
  | And | Or -> true
  | _ -> false

let str_of_val = function
  | Integer i -> string_of_int i
  | Boolean b -> string_of_bool b

let str_of_type = function
| Integer _ -> "int"
| Boolean _ -> "bool"

let rev_op = function
  | Ge -> Lt
  | Le -> Gt
  | Eq -> Ne
  | Ne -> Eq
  | Lt -> Ge
  | Gt -> Le
  | op -> op 

let label e =
    let rec l k = function
      | Void (_) -> Void (string_of_int k), k + 1
      | Const (c, _) -> Const (c, string_of_int k), k + 1
      | Var (x, _) -> Var (x, string_of_int k), k + 1
      | App (e1, e2, _) ->
          let e1', k1 = l k e1 in
          let e2', k2 = l k1 e2 in
          App (e1', e2', string_of_int k2), k2 + 1
      | Rec (fopt, x, _, e1, _) ->
          let fopt', k1 =
            fopt |>
            Opt.map (function (f, _) -> Some (f, string_of_int k), k + 1) |>
            Opt.get_or_else (None, k)
          in
          let e1', k2 = l (k1 + 1) e1 in
        Rec (fopt', x, string_of_int k1, e1', string_of_int k2), k2 + 1
      | Ite (e0, e1, e2, _) ->
          let e0', k0 = l k e0 in
          let e1', k1 = l k0 e1 in
          let e2', k2 = l k1 e2 in
          Ite (e0', e1', e2', string_of_int k2), k2 + 1
      | BinOp (bop, e1, e2, _) ->
        let e1', k1 = l k e1 in
        let e2', k2 = l k1 e2 in
        BinOp (bop, e1', e2', string_of_int k2), k2 + 1
    in
    l 0 e |> fst

let mk_int k = Const (Integer k, "")
let mk_bool b = Const (Boolean b, "")
let mk_var x = Var (x, "")
let mk_app e1 e2 = App (e1, e2, "")
let mk_lambda x e = Rec (None, x, "", e, "")
let mk_lambdas xs e = List.fold_right (fun x e -> mk_lambda x e) xs e
let mk_rec f x e = Rec (Some (f, ""), x, "", e, "")
let mk_ite e0 e1 e2 = Ite (e0, e1, e2, "")
let mk_op op e1 e2 = BinOp (op, e1, e2, "")

let mk_let_in x def e = mk_app (mk_lambda x e) def
let mk_lets defs e =
  List.fold_right (fun (x, def) e -> mk_let_in x def e) defs e

let lam_to_mu f = function
  | Rec(_, x, lx, e, le) -> Rec (Some (f, ""), x, lx, e, le)
  | _ -> raise (Invalid_argument "Invalid function lambda")

let mk_let_rec_in x def e = 
  mk_app (mk_lambda x e) (lam_to_mu x def)