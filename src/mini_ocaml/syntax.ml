
open Util

(** {1} Type definitions for representing syntax *)

(** Variable names *)
type var = string

(** Binary infix operators *)
type binop =
  | Plus  (* + *)
  | Ge    (* >= *)
(*
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
*)
type value =
  | Int of int
  | Bool of bool

(** Terms *)
type term =
  | Const of value * pos               (* i (int constant) *)
  | Var of var * pos                   (* x (variable) *)
  | App of term * term * pos           (* t1 t2 (function application) *)
  | BinOp of binop * term * term * pos (* t1 bop t2 (binary infix operator) *)
  | Ite of term * term * term * pos    (* if t1 then t2 else t3 (conditional) *)
  | Rec of (var * pos) option * var * pos * exp * pos (*lambda and recursive function*)

(*TODO: Add remain syntax features*)
  (*| Closure of var * term (* fun x -> t *) *)

let loc = function
  | Const (_, l)
  | Var (_, l)
  | App (_, _, l)
  | BinOp (_, _, _, l)
  | Ite (_, _, _, l)
  | Rec (_, _, _, _, l) -> l
