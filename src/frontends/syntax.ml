open Util

(** {1} Type definitions for representing syntax *)
type pos = { pl: int; pc: int }

let mk_default_loc = {pl=0;pc=0}

(** Variable names *)

type var = string
type loc = string

type asst = { isast: bool; ps: pos }

exception Input_Assert_failure of string

let fail pos msg =
  failwith (Printf.sprintf "Error: line %d col %d: %s" pos.pl pos.pc msg)

let print_loc pos = 
  raise (Input_Assert_failure 
  ("The program may not be safe, assertion failed at line "^(string_of_int pos.pl)^" col "^
  (string_of_int pos.pc)^".\n" ))

let construct_asst ps = match ps with
  | None -> { isast = false; ps = mk_default_loc }
  | Some ps -> { isast = true; ps = ps }

exception Main_not_found of string

(** Unary operators *)
type unop =
  | UMinus (* - *)
  | Not (* not *)
    
(** Binary infix operators *)
type binop =
  | Plus  (* + *)
  | Ge    (* >= *)
  | Mult  (* * *)
  | Div   (* / *)
  | Mod   (* mod *)
  | Modc  (* mod const *)
  | Minus (* - *)
  | Eq    (* = *)
  | Ne    (* <> *)
  | Lt    (* < *)
  | Gt    (* > *)
  | Le    (* <= *)
  | And   (* && *)
  | Or    (* || *)
  | Cons  (* :: *)

type value =
  | Integer of int
  | Boolean of bool
  | IntList of int list
  | Unit of unit

type inputType = Int | Bool | Unit

module VarDefMap = Map.Make(struct
  type t = var
  let compare = compare
end)

module ThresholdsSetType = Set.Make(struct
  type t = int
  let compare = compare
  end)

let thresholdsSet = ref ThresholdsSetType.empty

type pre_exp = {name: var; dtype: inputType; left: var; op: binop; right: var}

let string_of_op = function
  | Plus  (* + *) -> "+"
  | Ge    (* >= *) -> ">="
  | Mult  (* * *) -> "*"
  | Div   (* / *) -> "/"
  | Mod   (* mod *) -> "%"
  | Modc  (* mod const *) -> "mod"
  | Minus (* - *) -> "-"
  | Eq    (* = *) -> "="
  | Ne    (* <> *) -> "!="
  | Lt    (* < *) -> "<"
  | Gt    (* > *) -> ">"
  | Le    (* <= *) -> "<="
  | And   (* && *) -> "&&"
  | Or    (* || *) -> "||"
  | Cons  (* :: *) -> "::"

let string_of_unop = function
  | UMinus (* - *) -> "-"
  | Not (* not *) -> "not"
        
let op_of_string = function
  |  "+" -> Plus  (* + *) 
  | ">=" -> Ge    (* >= *) 
  | "*"  -> Mult  (* * *)
  |   "/" -> Div   (* / *)
  |  "mod"-> Mod   (* mod *)
  |  "-" -> Minus (* - *)
  |  "=" -> Eq    (* = *)
  |  "<>" -> Ne    (* <> *)
  |  "<" -> Lt    (* < *)
  |  ">" -> Gt    (* > *)
  | "<=" -> Le    (* <= *)
  |  "&&" -> And   (* && *)
  |  "||" -> Or    (* || *)
  |  "::" -> Cons  (* :: *)
  | s -> raise (Invalid_argument (s^": Invalid operator inside pre-defined var"))

let unop_of_string = function
  | "-" -> UMinus
  | "not" -> Not
  | s -> raise (Invalid_argument (s^": Invalid unary operator inside pre-defined var"))
        
let is_mod = function
  | Mod -> true
  | _ -> false

let is_mod_const = function
  | Modc -> true
  | _ -> false

let init_ref n d l o r = {name = n; dtype = d; left = l; op=op_of_string o; right = r}

let pre_vars: pre_exp VarDefMap.t ref = ref VarDefMap.empty

let string_to_type = function
  | "Bool" -> Bool
  | "Int" -> Int
  | "Unit" -> Unit
  | _ -> raise (Invalid_argument "Predefined type should contain either Int or Bool")

let type_to_string = function
  | Bool -> "Bool"
  | Int -> "Int"
  | Unit -> "Unit"

(** Terms *)
type term =
  | TupleLst of (term list) * loc         (* tuple list *)
  | Const of value * loc               (* i (int constant) *)
  | Var of var * loc                   (* x (variable) *)
  | App of term * term * loc           (* t1 t2 (function application) *)
  | UnOp of unop * term * loc          (* uop t (unary operator) *)
  | BinOp of binop * term * term * loc (* t1 bop t2 (binary infix operator) *)
  | PatMat of term * (patcase list) * loc     (* match t1 with t2 -> t3 | ... *)
  | Ite of term * term * term * loc * asst    (* if t1 then t2 else t3 (conditional) *)
  | Rec of (term) option * term * term * loc (*lambda and recursive function*)
and patcase = 
  | Case of term * term

let loc = function
  | TupleLst (_, l)
  | Const (_, l)
  | Var (_, l)
  | App (_, _, l)
  | BinOp (_, _, _, l)
  | UnOp (_, _, l)
  | Ite (_, _, _, l, _)
  | PatMat (_, _, l)
  | Rec (_, _, _, l) -> l

let cond_op = function
  | Plus | Mult | Div | Mod | Modc | Minus | And | Or | Cons -> false
  | Ge | Eq | Ne | Lt | Gt | Le -> true

let bool_op = function
  | And | Or -> true
  | _ -> false

let list_op = function
  | Cons -> true
  | _ -> false

let is_const = function
  | Const (_, _) -> true
  | _ -> false

let is_var = function
  | Var (_,_) -> true
  | _ -> false

let is_func = function
  | Rec (_) -> true
  | _ -> false

let is_asst_false = function
  | Const (c,_) -> (match c with
    | Boolean b -> b = false
    | _ -> false)
  | _ -> false

let str_of_const = function
  | Const (d, _) -> (match d with
    | Integer i -> string_of_int i
    | _ -> raise (Invalid_argument ("Expected mod <const> is an integer.")))
  | _ -> raise (Invalid_argument ("Expected mod <const> procedure for apron."))

let str_of_val = function
  | Integer i -> string_of_int i
  | Boolean b -> string_of_bool b
  | IntList lst -> let rec string_of_intlst = function
        | [] -> ""
        | [hd] -> (string_of_int hd)
        | hd::tl -> (string_of_int hd) ^ ";" ^ (string_of_intlst tl) in
        "[" ^  (string_of_intlst lst) ^ "]"
  | Unit _ -> "()"

let str_of_type = function
| Integer _ -> "int"
| Boolean _ -> "bool"
| IntList _ -> "int list"
| Unit _ -> "unit"

let is_array_set = function
  | Var (v, _) -> if v = "Array.set" then true else false
  | _ -> false

let get_var_name = function
  | Var (v, _) -> v
  | _ -> raise (Invalid_argument ("Expected expression is a variable"))

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
      | TupleLst (e, _) ->
        let e', k' = List.fold_left (fun (ne, nk) ie ->
          let ie', nk' = l nk ie in 
          ie' :: ne, nk' ) ([],k) e in
        let e' = List.rev e' in
        TupleLst (e', string_of_int k'), k' + 1
      | Const (c, _) -> Const (c, string_of_int k), k + 1
      | Var (x, _) -> Var (x, string_of_int k), k + 1
      | App (e1, e2, _) ->
          let e1', k1 = l k e1 in
          let e2', k2 = l k1 e2 in
          App (e1', e2', string_of_int k2), k2 + 1
      | Rec (fopt, px, e1, _) ->
          let fopt', k1 =
            fopt |>
            Opt.map (function (pf) -> let pf', kf' = l k pf in
              Some (pf'), kf') |>
            Opt.get_or_else (None, k)
          in
          let px', kx' = l k1 px in
          let e1', k2 = l kx' e1 in
        Rec (fopt', px', e1', string_of_int k2), k2 + 1
      | Ite (e0, e1, e2, _, b) ->
          let e0', k0 = l k e0 in
          let e1', k1 = l k0 e1 in
          let e2', k2 = l k1 e2 in
          Ite (e0', e1', e2', string_of_int k2, b), k2 + 1
      | PatMat (e, patlst, _) ->
          let e', k' = l k e in
          let patlst', k'' = lp k' patlst in
          PatMat (e', patlst', string_of_int k''), k'' + 1
      | UnOp (uop, e1, _) ->
        let e1', k1 = l k e1 in
        UnOp (uop, e1', string_of_int k1), k1 + 1
      | BinOp (bop, e1, e2, _) ->
        let e1', k1 = l k e1 in
        let e2', k2 = l k1 e2 in
        BinOp (bop, e1', e2', string_of_int k2), k2 + 1
    and lp k = function
      | [] -> [], k
      | Case(e1, e2) :: tl -> 
        let e1', k1 = l k e1 in
        let e2', k2 = l k1 e2 in 
        let e, k3 = Case(e1', e2'), k2 in
        let tl', k' = lp k3 tl in
        e :: tl', k'
    in
    l 0 e |> fst

let mk_int k = Const (Integer k, "")
let mk_bool b = Const (Boolean b, "")
let mk_var x = Var (x, "")
let mk_app e1 e2 = App (e1, e2, "")
let mk_pattern_case ep eval = Case (ep, eval)
let mk_lambda x e = Rec (None, Var (x, ""), e, "")
let mk_lambdas xs e = List.fold_right (fun x e -> mk_lambda x e) xs e
let mk_rec f x e = Rec (Some (Var (f, "")), Var (x, ""), e, "")
let mk_ite e0 e1 e2 = Ite (e0, e1, e2, "", construct_asst None)
let mk_op op e1 e2 = BinOp (op, e1, e2, "")
let mk_unop uop e1 = UnOp (uop, e1, "")

let mk_let_in x def e = mk_app (mk_lambda x e) def
let mk_lets defs e =
  List.fold_right (fun (x, def) e -> mk_let_in x def e) defs e

let lam_to_mu f = function
  | Rec(_, px, e, le) -> Rec (Some (Var (f, "")), px, e, le)
  | _ -> raise (Invalid_argument "Invalid function lambda")

let mk_let_rec_in x def e = 
  mk_app (mk_lambda x e) (lam_to_mu x def)

let pr_ary ppf ary = Array.fold_left (fun a e -> Format.printf "%s " e) () ary

let mk_pre_apps xs e = List.fold_right (fun x e -> mk_app e (mk_var ("pref"^x))) xs e

let mk_main_call x params = 
  let lst = List.rev params in
    mk_pre_apps lst (mk_var x)

let mk_let_main x def params = 
  let x' = if x = "_" then "main" else x in
  let e = let lst = List.rev params in
    mk_pre_apps lst (mk_var x')
  in
  mk_app (mk_lambda x e) def

let mk_let_main_rec x def params =
  let e = let lst = List.rev params in
    mk_pre_apps lst (mk_var x)
  in
  mk_app (mk_lambda x e) (lam_to_mu x def)

let mk_pattern_lambda t e = Rec (None, t, e, "")

let mk_pattern_let_in t def e = mk_app (mk_pattern_lambda t e) def
