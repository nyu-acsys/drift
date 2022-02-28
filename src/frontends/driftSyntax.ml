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
  | Seq   (* ; *)
      
type value =
  | Integer of int
  | Boolean of bool
  | IntList of int list
  | UnitLit

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
  | Seq   (* ;  *) -> ";"
        
let string_of_unop = function
  | UMinus (* - *) -> "-"

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
  |  ";" -> Seq (* ; *)
  | s -> raise (Invalid_argument (s^": Invalid operator inside pre-defined var"))

let unop_of_string = function
  | "-" -> UMinus
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
  | "bool" -> Bool
  | "int" -> Int
  | "unit" -> Unit
  | other -> raise (Invalid_argument ("Invalid base type '" ^ other ^ "'. Should be one of `bool`, `int`, or `unit`"))

let type_to_string = function
  | Bool -> "bool"
  | Int -> "int"
  | Unit -> "unit"

(** Terms *)
type term =
  | TupleLst of term list * loc         (* tuple list *)
  | Const of value * loc               (* i (int constant) *)
  | Var of var * loc                   (* x (variable) *)
  | App of term * term * loc           (* t1 t2 (function application) *)
  | UnOp of unop * term * loc          (* uop t (unary operator) *)
  | BinOp of binop * term * term * loc (* t1 bop t2 (binary infix operator) *)
  | PatMat of term * patcase list * loc     (* match t1 with t2 -> t3 | ... *)
  | Ite of term * term * term * loc * asst    (* if t1 then t2 else t3 (conditional) *)
  | Rec of (var * loc) option * (var * loc) * term * loc (*lambda and recursive function*)
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
  | Plus | Mult | Div | Mod | Modc | Minus | And | Or | Cons | Seq -> false
  | Ge | Eq | Ne | Lt | Gt | Le -> true

let bool_op = function
  | And | Or -> true
  | _ -> false

let is_list_op = function
  | Cons -> true
  | _ -> false

let is_const = function
  | Const (_, _) -> true
  | _ -> false

let is_var = function
  | Var (_,_) -> true
  | _ -> false

let is_var_x x = function
  | Var (y,_) -> y = x
  | _ -> false

let is_func = function
  | Rec _ -> true
  | _ -> false

let is_tuple = function
  | TupleLst _ -> true
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
  | UnitLit -> "()"

let str_of_type = function
| Integer _ -> "int"
| Boolean _ -> "bool"
| IntList _ -> "int list"
| UnitLit -> "unit"

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
      | Rec (fopt, (x, _), e1, _) ->
          let fopt', k1 =
            fopt |>
            Opt.map (function (f, _) -> let fl', k' = (f, string_of_int k), k + 1 in
            Some fl', k') |>
            Opt.get_or_else (None, k)
          in
          let px', kx' = (x, string_of_int k1), k1 + 1 in
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
      | Case (e1, e2) :: tl -> 
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

let fresh_var =
  let used_names = Hashtbl.create 0 in
  fun  (name : string) ->
    let last_index = 
      Hashtbl.find_opt used_names name |>
      Opt.get_or_else (-1)
    in
    let new_index = last_index + 1 in
    Hashtbl.replace used_names name new_index;
    name ^ (string_of_int new_index)
  
    
let fv_acc acc e =
  let rec fv bvs acc = function
    | Const _ -> acc
    | Var (x, _) ->
        if StringSet.mem x bvs
        then acc
        else StringSet.add x acc
    | App (e1, e2, _) 
    | BinOp (_, e1, e2, _) -> List.fold_left (fv bvs) acc [e1; e2]
    | UnOp (_, e, _) -> fv bvs acc e
    | Ite (b, t, e, _, _) -> List.fold_left (fv bvs) acc [b; t; e]
    | PatMat (t, ps, _) ->
        let acc1 = fv bvs acc t in
        List.fold_left (fun acc (Case (p, t)) ->
          let bvs1 = fv StringSet.empty bvs p in
          fv bvs1 acc t)
          acc1 ps
    | TupleLst (ts, _) ->
        List.fold_left (fv bvs) acc ts
    | Rec (f_opt, (x, _), e, _) ->
        let d = StringSet.of_list (x :: (f_opt |> Opt.map fst |> Opt.to_list)) in
        fv (StringSet.union bvs d) acc e
  in
  fv StringSet.empty acc e

let fv e = fv_acc StringSet.empty e
    
let fo e =
  let inc acc x = StringMap.update x (function Some x -> Some (x + 1) | None -> Some 1) acc in
  let rec fv bvs acc = function
    | Const _ -> acc
    | Var (x, _) ->
        if StringMap.mem x bvs
        then acc
        else inc acc x
    | App (e1, e2, _) 
    | BinOp (_, e1, e2, _) -> List.fold_left (fv bvs) acc [e1; e2]
    | UnOp (_, e, _) -> fv bvs acc e
    | Ite (b, t, e, _, _) -> List.fold_left (fv bvs) acc [b; t; e]
    | PatMat (t, ps, _) ->
        let acc1 = fv bvs acc t in
        List.fold_left (fun acc (Case (p, t)) ->
          let bvs1 = fv StringMap.empty bvs p in
          fv bvs1 acc t)
          acc1 ps
    | TupleLst (ts, _) ->
        List.fold_left (fv bvs) acc ts
    | Rec (f_opt, (x, _), e, _) ->
        let bvs1 = List.fold_left inc bvs (x :: (f_opt |> Opt.map fst |> Opt.to_list)) in
        fv bvs1 acc e
  in
  fv StringMap.empty StringMap.empty e
    
let mk_lambda p e =
  match p with
  | Var (x, _) -> Rec (None, (x, ""), e, "")
  | t ->
      let x = fresh_var "x" in
      let e' = PatMat (Var (x, ""), [mk_pattern_case p e], "") in
      Rec (None, (x, ""), e', "")
        
let mk_lambdas ps e = List.fold_right mk_lambda ps e
let mk_rec f x e =
  let f_opt = 
    if StringSet.mem f (fv e) then Some (f, "") else None
  in
  Rec (f_opt, (x, ""), e, "")
let mk_ite e0 e1 e2 = Ite (e0, e1, e2, "", construct_asst None)
let mk_op op e1 e2 = BinOp (op, e1, e2, "")
let mk_unop uop e1 = UnOp (uop, e1, "")

let mk_let_in x def e =
  match x with
  | Var (x', _) when false && not @@ StringSet.mem x' (fv e) ->
      BinOp (Seq, def, e, "")
  | _ -> mk_app (mk_lambda x e) def

let mk_lets defs e =
  List.fold_right (fun (x, def) e -> mk_let_in x def e) defs e

let lam_to_mu f = function
  | e when not @@ StringSet.mem f (fv e) ->
      e
  | Rec(None, px, e, le) -> Rec (Some (f, ""), px, e, le)
  | _ -> raise (Invalid_argument "Invalid function lambda")

let mk_let_rec_in x def e = 
  mk_app (mk_lambda (Var (x, "")) e) (lam_to_mu x def)

let pr_ary ppf ary = Array.fold_left (fun a e -> Format.printf "%s " e) () ary

let mk_pre_apps xs e = List.fold_right (fun x e -> mk_app e (mk_var ("pref"^x))) xs e

let mk_main_call x params = 
  let lst = List.rev params in
    mk_pre_apps lst (mk_var x)

let mk_let_main x def params = 
  let x' = if x = "_" then "main" else x in
  match params with
  | [] -> def
  | _ ->
      let e =
        let lst =
          List.rev_map (function Var (x, _) -> x | _ -> failwith "parameters of main function can only be variables")
            params
        in
        mk_pre_apps lst (mk_var x')
      in
      mk_app (mk_lambda (Var (x', "")) e) def

let mk_let_main_rec x def params =
  match params with
  | [] ->
      lam_to_mu x def
  | _ ->
      let e =
        let lst = List.rev params in
        mk_pre_apps lst (mk_var x)
      in
      mk_app (mk_lambda (Var (x, "")) e) (lam_to_mu x def)

let mk_pattern_lambda t e = Rec (None, t, e, "")

let mk_pattern_let_in t def e = mk_app (mk_pattern_lambda t e) def

(** Substitute closed term c for free occurrences of x in e (not capture avoiding if c is not closed) *)
let subst sm =
  let update_sm bvs sm =
    let sm1 =
      StringSet.fold StringMap.remove bvs sm
    in
    let fv_sm = StringMap.fold (fun _ t acc -> fv_acc acc t) sm1 StringSet.empty in
    let sm2 =
      StringSet.fold
        (fun x sm2 ->
          if StringSet.mem x fv_sm then
            let x1 = fresh_var x in
            StringMap.add x (Var (x1, "")) sm2
          else sm2)
        bvs sm1
    in
    sm2
  in
  let subst_bv sm (x, l) =
    StringMap.find_opt x sm |>
    Opt.map (function (Var (x1, _)) -> x1 | _ -> x) |>
    Opt.get_or_else x, l
  in
  let rec subst sm e =
    let s = subst sm in
    match e with
      | Const _ as e -> e
      | Var (y, _) as v ->
          StringMap.find_opt y sm |> Opt.get_or_else v
        | App (e1, e2, l) ->
            App (s e1, s e2, l)
        | BinOp (bop, e1, e2, l) ->
            BinOp (bop, s e1, s e2, l)
        | UnOp (uop, e, l) ->
            UnOp (uop, s e, l)
        | TupleLst (ts, l) ->
            TupleLst (List.map s ts, l)
        | Ite (b, t, e, l, a) ->
            Ite (s b, s t, s e, l, a)
        | Rec (f_opt, y, e, l) as r ->
            let bvs = fst y :: (f_opt |> Opt.map fst |> Opt.to_list) |> StringSet.of_list in
            let sm1 = update_sm bvs sm in
            if sm1 = StringMap.empty then r else
            let f_opt1 = f_opt |> Opt.map (subst_bv sm1) in
            let y1 = subst_bv sm1 y in
            let e1 = subst sm1 e in
            Rec (f_opt1, y1, e1, l)
        | PatMat (t, ps, l) ->
            let ps1 =
              List.map (function Case (p, t) as c ->
                let sm1 = update_sm (fv p) sm in
                if sm1 = StringMap.empty then c else
                let p1 = subst sm1 p in
                let t1 = subst sm1 t in
                Case (p1, t1))
                ps
            in
            PatMat (s t, ps1, l)
  in subst sm
    
let simplify =
  let fo x c =
    fo c |>
    StringMap.find_opt x |>
    Opt.get_or_else 0
  in
  let rec simp = function
  | (Var _ | Const _) as e -> e
  | App (e1, e2, l) ->
      (match simp e1, simp e2 with
      | Rec (None, (x, _), e, _), (Const _ | Var _ as c) ->
          let a1 = subst (StringMap.singleton x c) e in
          simp a1
      | Rec (None, (x, _), e, _), (Rec _ as c)
        when fo x e < 2 ->
          let a1 = subst (StringMap.singleton x c) e in
          simp a1
      | e1', e2' -> App (e1', e2', l))
  | BinOp (bop, e1, e2, l) ->
      (match bop, simp e1 with
      | Seq, (Const _ | Var _ | Rec _) -> simp e2
      | _, se1 -> BinOp (bop, se1, simp e2, l))
  | UnOp (uop, e, l) ->
      UnOp (uop, simp e, l)
  | Rec (f_opt, (x, xl), e, l) ->
      Rec (f_opt, (x, xl), simp e, l)
  | TupleLst (ts, l) ->
      TupleLst (List.map simp ts, l)
  | PatMat (e1, ps, l) ->
      let ps' = List.map (function Case (p, t) -> Case (p, simp t)) ps in
      PatMat (simp e1, ps', l)
  | Ite (b, t, e, l, a) ->
      Ite (simp b, simp t, simp e, l, a)
  in simp
