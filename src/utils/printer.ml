open Syntax
open EffectAutomataSyntax
open AbstractDomain
open SensitiveDomain
open SenSemantics
open TracePartDomain

(** Pretty printing *)
let pr_relation ppf = function
  | Bool (vt, vf) -> Format.fprintf ppf "@[<1>{@ cur_v:@ Bool@ |@ TRUE:@ %a,@ FALSE:@ %a@ }@]"  AbstractValue.print_abs vt AbstractValue.print_abs vf
  | Int v -> Format.fprintf ppf "@[<1>{@ cur_v:@ Int@ |@ %a@ }@]" AbstractValue.print_abs v
  | Unit v -> Format.fprintf ppf "@[<1>{@ Unit@ |@ %a@ }@]" AbstractValue.print_abs v
  | Env v -> Format.fprintf ppf "@[<1>%a@]" AbstractValue.print_abs v

let pr_label pl ppf l = if pl then Format.fprintf ppf "^%s" l else ()

let pr_const ppf value = 
  match value with
  | Integer i when i < 0 -> Format.fprintf ppf "(%s)" (str_of_val value)
  | _ -> Format.fprintf ppf "%s" (str_of_val value)

let pr_op ppf op = Format.fprintf ppf "%s" (string_of_op op)

let pr_unop ppf op = Format.fprintf ppf "%s" (string_of_unop op)

let rec pr_exp pl ppf = function
| TupleLst (u, l) -> 
    let rec print_tuple pl ppf = function
      | [] -> ()
      | [e] -> Format.fprintf ppf "@[<1>%a@]" 
        (pr_exp pl) e
      | e :: tl -> 
        Format.fprintf ppf "@[<2>%a,@ %a@]" 
        (pr_exp pl) e
        (print_tuple pl) tl
    in
    Format.fprintf ppf "Tup @[<2>(%a)%a@]"
      (print_tuple pl) u (pr_label pl) l
| Const (c, l) ->
    Format.fprintf ppf "%a%a" pr_const c (pr_label pl) l
| Var (x, l) ->
    Format.fprintf ppf "%s%a" x (pr_label pl) l
| App (e1, e2, l) ->
    Format.fprintf ppf "@[<2>(%a@ %a)%a@]"
      (pr_exp pl) e1
      (pr_exp pl) e2
      (pr_label pl) l
| Rec (None, (x, lx), e, l) ->
    Format.fprintf ppf "@[<2>(lambda %s%a.@ %a)%a@]"
      x (pr_label pl) lx
      (pr_exp pl) e
      (pr_label pl) l
| Rec (Some (f, lf), (x, lx), e, l) ->
    Format.fprintf ppf "@[<2>(mu %s%a %s%a.@ %a)%a@]"
      f (pr_label pl) lf
      x (pr_label pl) lx
      (pr_exp pl) e
      (pr_label pl) l
| Ite (e1, e2, e3, l) ->
    Format.fprintf ppf "Ite @[<2>(%a@ ?@ %a@ :@ %a)%a@]"
      (pr_exp pl) e1
      (pr_exp pl) e2
      (pr_exp pl) e3
      (pr_label pl) l
| BinOp (bop, e1, e2, l) ->
    Format.fprintf ppf "@[<2>(%a@ %a@ %a)%a@]"
    (pr_exp pl) e1
    pr_op bop
    (pr_exp pl) e2
    (pr_label pl) l
| UnOp (uop, e1, l) ->
    Format.fprintf ppf "@[<2>(%a@ %a)%a@]"
    pr_unop uop
    (pr_exp pl) e1
    (pr_label pl) l
| PatMat (e, patlst, l) ->
  Format.fprintf ppf "@[<2>(match@ %a@ with@ %a)%a@]"
    (pr_exp pl) e
    (pr_pm pl) patlst
    (pr_label pl) l
| Event (e, l) -> 
   Format.fprintf ppf "@[<2>(ev@ %a)%a@]"
     (pr_exp pl) e
     (pr_label pl) l
| Assert (e, _, l) ->
   Format.fprintf ppf "@[<2>(assert@ %a)%a@]"
     (pr_exp pl) e
     (pr_label pl) l
| NonDet l ->
   Format.fprintf ppf "nondet%a" (pr_label pl) l
and pr_pm pl ppf = function
  | [] -> ()
  | [c] -> Format.fprintf ppf "%a" (pr_pattern pl) c
  | hd::tl -> Format.fprintf ppf "%a@ |@ %a" (pr_pattern pl) hd (pr_pm pl) tl
and pr_pattern pl ppf (Case (e1, e2)) = 
  Format.fprintf ppf "@[<2>%a@ ->@ %a@]" (pr_exp pl) e1 (pr_exp pl) e2

let print_exp out_ch e =
  let ppf = Format.formatter_of_out_channel out_ch in 
  Format.pp_set_geometry ppf ~max_indent:190 ~margin:200;
  Format.fprintf ppf "%a@?" (pr_exp true) e

let loc_of_node n = get_label_snode n

let pr_node ppf n = Format.fprintf ppf "%s" (loc_of_node n)

let rec pr_node_full ppf n = print_node n ppf pr_env
and pr_env ppf = function
  | [] -> ()
  | [x, (n,r)] -> Format.fprintf ppf "%s, %b: %a" x r pr_node_full n
  | (x, (n,r)) :: env -> Format.fprintf ppf "%s, %b: %a,@ %a" x r pr_node_full n pr_env env

let rec pr_env_vars ppf = function
  | [] -> ()
  | [x, _] -> Format.fprintf ppf "%s" x
  | (x, _) :: env -> Format.fprintf ppf "%s,@ %a" x pr_env_vars env

let string_of_node n = pr_node Format.str_formatter n; Format.flush_str_formatter ()

let pr_agg_val ppf a = match a with
  | Bool (vt, vf) -> Format.fprintf ppf "@[<1>@ TRUE:@ %a,@ FALSE:@ %a@ @]"  AbstractValue.print_abs vt AbstractValue.print_abs vf
  | Int v | Unit v | Env v -> Format.fprintf ppf "@[<1>@ %a@ @]" AbstractValue.print_abs v

let pr_ary ppf ary = 
  let (l,e), (rl, ve) = ary in
  Format.fprintf ppf "@[<1>{@ cur_v:@ Int Array (%s, %s)@ |@ len:@ %a,@ item:@ %a@ }@]" l e pr_agg_val rl pr_agg_val ve

let rec shape_value = function
  | Bot -> "Bot"
  | Top -> "Top"
  | Relation r -> (match r with
    | Int _ -> "Int"
    | Bool _ -> "Bool"
    | Unit _ -> "Unit"
    | Env _ -> "Env")
  | Table t -> let (_, (vei,veo), _) = get_full_table_T t in 
              (shape_value_and_eff vei)^"->"^(shape_fout veo)
  | Tuple u -> if List.length u = 0 then "Unit"
    else 
      let rec shape_tuple = function
      | [] -> ""
      | [it] -> shape_value_and_eff it
      | hd :: tl -> (shape_value_and_eff hd) ^ "*" ^ (shape_tuple tl) in
      shape_tuple u
  | Ary _ -> "Array"
  | Lst _ -> "List"
and shape_eff = function 
  | EffBot -> "Bot"
  | EffTop -> "Top"
  | Effect e -> StateMap.bindings e
               |> List.map (fun ((Q q), acc) ->
                      (string_of_int q) ^ "|-> {" ^
                        (match acc with 
                        | Int _ -> "Int"
                        | Bool _ -> "Bool"
                        | Unit _ -> "Unit"
                        | Env _ -> "Env")
                        ^ "}")
               |> String.concat " ; "
and shape_value_and_eff = function 
  | TEBot -> "Bot"
  | TETop -> "Top"
  | TypeAndEff (v, e) -> "("^ (shape_value v) ^","^ (shape_eff e) ^ ")"
and shape_fout fout = let (_, vo) = get_full_fout fout in shape_value_and_eff vo

let rec pr_value ppf v = match v with
  | Bot -> Format.fprintf ppf "_|_"
  | Top -> Format.fprintf ppf "T"
  | Relation r -> pr_relation ppf r
  | Table t -> print_table t ppf pr_value_and_eff pr_relation
  | Tuple u -> Format.fprintf ppf "(%a)" pr_tuple u
  | Ary ary -> pr_ary ppf ary
  | Lst lst -> pr_lst ppf lst
and pr_value_and_eff ppf ve = match ve with
  | TEBot -> Format.fprintf ppf "_|_"
  | TETop -> Format.fprintf ppf "T"
  | TypeAndEff (v, e) -> 
     if (not !Config.ev_trans) then Format.fprintf ppf "@[(@ @[<v>@[t: %a@],@ @[<v>eff: @[<v>%a@]@]@])@]" pr_value v pr_eff e
     else
       pr_value ppf v
and pr_eff_map ppf e = 
  if StateMap.is_empty e then Format.fprintf ppf "Empty" 
  else StateMap.bindings e 
       |> Format.pp_print_list ~pp_sep: (fun _ () -> Format.printf ";@ ") pr_eff_binding ppf
and pr_eff ppf eff = match eff with 
  | EffBot -> Format.fprintf ppf "_|_"
  | EffTop -> Format.fprintf ppf "T"
  | Effect e -> pr_eff_map ppf e
and pr_eff_acc ppf acc = 
  Format.fprintf ppf "@[<1>%a@]" pr_relation acc
and pr_eff_acc_binding ppf (_, r) =
  (* Format.fprintf ppf "@[<1>(@ %s@ |->@ %a)@]" v pr_relation r *)
  Format.fprintf ppf "@[<1>%a@]" pr_relation r
and pr_eff_binding ppf ((Q q), acc) = 
  Format.fprintf ppf "@[<1>(@ %s@ |->@ @[<v>%a@])@]" (string_of_int q) pr_eff_acc acc
and pr_lst ppf lst =
    let (l,e), (rl, ve) = lst in
    Format.fprintf ppf "@[<1>{@ cur_v:@ %s List (%s, %s)@ |@ len:@ %a,@ item:@ %a@ }@]" (shape_value_and_eff ve) l e pr_agg_val rl pr_value_and_eff ve
and pr_tuple ppf u = 
  if List.length u = 0 then Format.fprintf ppf "@[<1>Unit@]"
  else 
    let rec print_list ppf = function
    | [] -> ()
    | [it] -> pr_value_and_eff ppf it
    | hd :: tl -> 
    Format.fprintf ppf "@[<1>%a,@ %a @]" pr_value_and_eff hd print_list tl in
    print_list ppf u
and pr_fout ppf fout = print_fout fout ppf pr_value_and_eff

let rec pr_tails ppf tails = match tails with
  | [] -> ()
  | [(tr, ae)] -> Format.fprintf ppf "@[(%a, %a)]" print_trace tr pr_value ae
  | (tr, ae) :: tails -> Format.fprintf ppf "@[(%a, %a)]@" print_trace tr pr_value ae; pr_tails ppf tails

let sort_list (m: exec_map_t) =
  let lst = (NodeMap.bindings m) in
  List.sort (fun (n1,_) (n2,_) -> 
    compare_node n1 n2) lst 

let print_value out_ch v = 
  let ppf = Format.formatter_of_out_channel out_ch in 
  Format.pp_set_geometry ppf ~max_indent:190 ~margin:200;
  Format.fprintf ppf "%a@?" pr_value v

let string_of_value v = pr_value Format.str_formatter v; Format.flush_str_formatter ()
let string_of_value_and_eff ve = pr_value_and_eff Format.str_formatter ve; Format.flush_str_formatter ()

let rec pr_exec_map ppf m =
  Format.fprintf ppf "----\n%a----\n" pr_exec_rows (sort_list m)
and pr_exec_rows ppf = function
  | [] -> ()
  | [row] -> Format.fprintf ppf "%a\n" pr_exec_row row
  | row :: rows -> Format.fprintf ppf "%a@\n@\n%a" pr_exec_row row pr_exec_rows rows
and pr_exec_row ppf (n, v) =
  Format.fprintf ppf "@[<2>%a |->@ @[<2>%a@]@]" pr_node n pr_value_and_eff v

let print_exec_map m = Format.fprintf Format.std_formatter "%a@?" pr_exec_map m

let rec print_exps out_ch = function
| [] -> ()
| e :: tl -> print_exp out_ch e; print_exps out_ch tl

let rec str_toplist str = function 
| [] -> str
| [(vr,ty)] -> str^"("^vr^", "^(str_of_type ty)^")"
| (vr,ty)::l -> let s' = str^"("^vr^", "^(str_of_type ty)^"); " in (str_toplist s' l)

let pr_pre_exp as_type ppf = function
  | {name = n; dtype = d; left = l; op = _; right = r} ->
     if as_type then begin
       if l = "top" then Format.fprintf ppf "%s(*-:{%s:%s | %s}*)" 
                           (mltype_of_inputType d) n (type_to_string d) l
       else Format.fprintf ppf "%s(*-:{%s:%s | %s = %s}*)" 
              (mltype_of_inputType d) n (type_to_string d) l r end
     else begin
       if l = "top" then Format.fprintf ppf "{%s:%s | %s}" n (type_to_string d) l
       else Format.fprintf ppf "{%s:%s | %s = %s}" n (type_to_string d) l r end

let pr_pre_def_vars ppf = 
  let rec pr_ll = function
  | [] -> ()
  | (k, ls)::tl -> Format.fprintf ppf "@[<2>%s@ ->@ %a@]\n" k (pr_pre_exp false) ls; pr_ll tl
  in
  let ls = VarDefMap.bindings !pre_vars in
  pr_ll ls

let print_last_node m = 
  let pr_map_lst_node ppf m = let lst = (sort_list m) in 
    let lst_n, v = List.nth lst (List.length lst - 1) in
    Format.fprintf ppf "@[<2>%a |->@ @[<2>%a@]@]\n\n" pr_node lst_n pr_value_and_eff v
  in
  Format.fprintf Format.std_formatter "%a@?" pr_map_lst_node m

let print_node_by_label m l = 
  let pr_map_nst_node ppf m = let lst = (sort_list m) in
  try
    let nst_n, v = List.nth lst (l + 4) in
    Format.fprintf ppf "@[<2>%a |->@ @[<2>%a@]@]\n\n" pr_node nst_n pr_value_and_eff v
  with Failure _ -> ()
  in
  Format.fprintf Format.std_formatter "%a@?" pr_map_nst_node m


(* CPS conversion *)
let pr_cps_params ppf ps = Format.pp_print_list 
                             ~pp_sep:(fun ppf () -> Format.fprintf ppf " ") 
                             (fun ppf x -> Format.fprintf ppf "%s" x) ppf ps 
let pr_cps_acc ppf acc = pr_cps_params ppf acc

let pr_cps_op ppf op =
  match op with 
  | Mod -> Format.fprintf ppf "%s" "mod"
  | _ -> pr_op ppf op

let rec pr_cps_term ppf = function
  | Const (c, _) -> Format.fprintf ppf "%a" pr_const c
  | Var (x, _) -> Format.fprintf ppf "%s" x
  | App (e1, e2, _) -> Format.fprintf ppf "%a %a" pr_cps_term e1 pr_cps_term e2
  | Rec (None, (x, _), e, _) -> Format.fprintf ppf "fun %s -> @[<v 2>%a@]" x pr_cps_term e
  | Rec (Some (f, _), (x, _), e, _) -> Format.fprintf ppf "rec fun %s %s -> @[<v 2>%a@]" f x pr_cps_term e
  | Ite (e0, e1, e2, _) -> Format.fprintf ppf "if @[%a@] then @[<v>%a@] @;else @[<v>%a@]"
                            pr_cps_term e0 pr_cps_term e1 pr_cps_term e2
  | BinOp (bop, e1, e2, _) -> Format.fprintf ppf "(@[%a@] %a @[%a@])" 
                               pr_cps_term e1 pr_op bop pr_cps_term e2
  | UnOp (uop, e1, _) -> Format.fprintf ppf "(%a @[%a@])" pr_unop uop pr_cps_term e1
  | _ -> raise (Invalid_argument "Expression not supported in ev")
 
let rec pr_cps_kterm ppf = function
  | KLetVal (x, v, kt) -> Format.fprintf ppf "@[<v>@[<v 2>let %s = %a@] in @;@[%a@]@]" x pr_cps_kval v pr_cps_kterm kt
  | KLetCont (k, q, acc, x, kdef, kt) -> 
     Format.fprintf ppf "@[<v>@[<v 2>let %s %s %a %s =@ %a@] in @;@[%a@]@]" 
       k q pr_cps_acc acc x pr_cps_kterm kdef pr_cps_kterm kt
  | KContApp (k, q, acc, x) -> 
     Format.fprintf ppf "@[%s %s %a %s@]" k q pr_cps_acc acc (String.concat " " x)
  | KApp (f, k, q, acc, x) -> 
     Format.fprintf ppf "@[%s %s %s %a %s@]" f k q pr_cps_acc acc (String.concat " " x)
  | KIte (xcond, kt1, kt2) -> 
     Format.fprintf ppf "@[if %s@ then@ @[<2>%a@]@ else@ @[%a@]@]" xcond pr_cps_kterm kt1 pr_cps_kterm kt2 
  | KFix (f, k, q, acc, x, fdef, kt) ->
     Format.fprintf ppf "@[<v>@[<v 2>let rec %s %s %s %a %s =@;@[%a@]@] in @;@[%a@]@]" 
       f k q pr_cps_acc acc (String.concat " " x) pr_cps_kterm fdef pr_cps_kterm kt
  | KLetUnOp (x, op, o, kt) -> 
     Format.fprintf ppf "@[let %s = %a %s in @;@[%a@]@]" x pr_unop op o pr_cps_kterm kt
  | KLetBinOp (x, op, o1, o2, kt) -> 
     Format.fprintf ppf "@[let %s = %s %a %s in @;@[%a@]@]" x o1 pr_cps_op op o2 pr_cps_kterm kt
  | KEvApp (k, q, acc, x) -> 
     Format.fprintf ppf "@[%s %s %s %a %s@]" "ev" k q pr_cps_acc acc x
  | KEvAssertApp (k, q, acc, x) ->
     Format.fprintf ppf "@[%s %s %s %a %s@]" "ev_assert" k q pr_cps_acc acc x
  | KAssert (x, kt) ->  
     Format.fprintf ppf "@[assert(%s);@[%a@]@]" x pr_cps_kterm kt
  | KExp (e) -> 
     Format.fprintf ppf "@[<v>%a@]" pr_cps_term e
  | KExit x -> Format.fprintf ppf "%s" x
  | KMainDef (main_params, main_body) -> 
     Format.fprintf ppf "@[<v 2>let main %a = @;@[%a@]@]"
       pr_cps_params main_params pr_cps_kterm main_body
and pr_cps_kval ppf = function 
  | KConst c -> pr_const ppf c
  | KFn (k, q, acc, x, def) -> 
     Format.fprintf ppf "@[<v 2>fun %s %s %a %s ->@;@[%a@]@]" k q pr_cps_acc acc (String.concat " " x) pr_cps_kterm def
  | KRandomInt -> Format.fprintf ppf "@[Random.int(0)@]"


(* ML print *)

let pr_ml_op ppf op =
  match op with 
  | Mod -> Format.fprintf ppf "%s" "mod"
  | Modc -> Format.fprintf ppf "%s" "mod"
  | _ -> pr_op ppf op

let rec pr_mlterm ppf = function
  | MlConst c -> Format.fprintf ppf "%a" pr_const c
  | MlVar x -> Format.fprintf ppf "%s" x
  | MlApp (e1, e2) -> Format.fprintf ppf "(%a %a)" pr_mlterm e1 pr_mlterm e2
  | MlRec (None, xs, e) -> Format.fprintf ppf "(@[<v 2>fun %a ->@;%a@])" 
                            pr_mlrec_params xs pr_mlterm e
  | MlRec (Some f, xs, e) -> Format.fprintf ppf "(@[<v 2>let rec %s %a =@;%a@ in %s@])" 
                              f pr_mlrec_params xs pr_mlterm e f
  | MlIte (e0, e1, e2) -> Format.fprintf ppf 
                           "@[<v>(@[<hv 2>if@ @[%a@]@ then@;@[<v>%a@]@]@;@[<hv 2>else@;@[<v>%a@]@])@]"
                           pr_mlterm e0 pr_mlterm e1 pr_mlterm e2
  | MlBinOp (bop, e1, e2) -> Format.fprintf ppf "(@[%a@] %a @[%a@])" 
                              pr_mlterm e1 pr_ml_op bop pr_mlterm e2
  | MlUnOp (uop, e1) -> Format.fprintf ppf "(%a @[%a@])" pr_unop uop pr_mlterm e1
  | MlNonDet -> Format.fprintf ppf "(Random.int (0) > 0)"
  | MlLetIn (name, MlRec (Some f, xs, e1), e2) when name = f ->
     Format.fprintf ppf "@[<v 2>let rec %s@ %a =@;@[%a@] in @;@[%a@]@]"
       f pr_mlrec_params xs pr_mlterm e1 pr_mlterm e2
  | MlLetIn (name, e1, e2) -> Format.fprintf ppf "@[<v>let @[%s@] = @[%a@] in @;@[%a@]@]"
                               name pr_mlterm e1 pr_mlterm e2
  | MlPatMat (e, [MlCase (MlTupleLst [x1; x2], MlTupleLst [MlBinOp (Seq, _, x1'); x2'])])
       when x1 = x1' && x2 = x2' -> pr_mlterm ppf e
  | MlPatMat (e, pcs) -> Format.fprintf ppf "(@[<v>match @[%a@] with @;@[<v 2>%a@]@])"
                          pr_mlterm e pr_mlcases pcs
  | MlTupleLst xs -> Format.fprintf ppf "@[(%a)@]" pr_mltuple xs
  | MlGDefs (ges, Some main) -> Format.fprintf ppf "@[<v>%a @.@.@.%a@]"
                                 pr_mlgdefs ges pr_mlterm main
  | MlGDefMain (MlRec (_, xs, def)) -> Format.fprintf ppf "@[<v 2>let main %a =@;%a@]"
                                           pr_mlrec_main_params xs pr_mlterm def
  | MlAssert (e, _) -> Format.fprintf ppf "@[assert %a@]" pr_mlterm e
  | _ -> raise (Invalid_argument "Unexpected expression in the translated program")
and pr_mlrec_params ppf ps = Format.pp_print_list 
                               ~pp_sep:(fun ppf () -> Format.fprintf ppf " ") 
                               (fun ppf x -> Format.fprintf ppf "%a" pr_mlterm x) ppf ps
and pr_mlrec_main_params ppf ps = Format.pp_print_list
                                    ~pp_sep:(fun ppf () -> Format.fprintf ppf " ")
                                    (fun ppf x -> Format.fprintf ppf "(%a:%a)"
                                                 pr_mlterm x 
                                                 (pr_pre_exp true) 
                                                 (VarDefMap.find ("pref"^(var_of_mlterm x)) !pre_vars)) ppf ps 
and pr_mltuple ppf xs = Format.pp_print_list 
                          ~pp_sep:(fun ppf () -> Format.fprintf ppf ",")
                          (fun ppf x -> Format.fprintf ppf "%a" pr_mlterm x) ppf xs 
and pr_mlgdefs ppf ges = Format.pp_print_list
                           ~pp_sep:(fun ppf () -> Format.fprintf ppf "@.@.@.")
                           (fun ppf ge -> Format.fprintf ppf "@[<v>%a@]" pr_mlgdef ge) ppf ges
and pr_mlgdef ppf (name, e) = 
  match e with
  | MlRec (None, xs, e') -> Format.fprintf ppf "@[<v 2>let %s %a =@;%a@]" 
                             name pr_mlrec_params xs pr_mlterm e'
  | MlRec (Some f, xs, e') -> Format.fprintf ppf "@[<v 2>let rec %s %a =@;%a@]" 
                             f pr_mlrec_params xs pr_mlterm e'
  | _ -> Format.fprintf ppf "let %s %a" name pr_mlterm e 
  (* | _ -> raise (Invalid_argument "Unexpected expression in the translated program") *)
and pr_mlcases ppf pcs = Format.pp_print_list
                           ~pp_sep:(fun ppf () -> Format.fprintf ppf "@;")
                           (fun ppf (MlCase (p, e)) -> Format.fprintf ppf "@[@[%a@] -> @[%a@]"
                                                      pr_mlterm p pr_mlterm e) ppf pcs
