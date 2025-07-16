open Syntax
open Util

exception CFAError of string

(* Analysis dataflow lattice *)
module Fact = 
  struct
    (* Effect annotations are : 
       - CertNot : The evaluation of an expression will CertainlyNot observe an "ev" expr
       - Bot : not information
       - Top : Some evaluation will observe an "ev" expr
     *)
    type eff_t = CertNot | EBot | ETop

    (* (Most General) Type Values
       - Base 
       - Fun: t ->A t  (function is annotated with E for tracking possible effectful computation as annotations)
       - Tuple: t * ... * t 
       - Bot
       - Top
       note the 
     *)
    type ty_t =  TBase | TFun of ty_t * ty_t * eff_t | TTuple of ty_t list | TBot | TTop

    (* Analysis domain *)
    type t = Bot | Top | Val of ty_t * eff_t

    let join_eff a1 a2 = 
      match a1, a2 with
      | EBot , a | a , EBot -> a 
      | ETop , a | a , ETop -> ETop
      | _ -> CertNot 

    let leq_eff a1 a2 =
      match a1, a2 with
      | EBot, _ -> true
      | _ , ETop -> true
      | CertNot, CertNot -> true
      | _, _ -> false

    (* pretty printing *)
    let pr_eff ppf = function
      | EBot -> Format.fprintf ppf "\u{22A5}\u{2091}" (* \bot_e *)
      | CertNot -> Format.fprintf ppf "T"             (* T from Trivial, i.e., no ev occurs in the eval *)
      | ETop -> Format.fprintf ppf "E"                (* E from Effectful, i.e., ev may occur in the eval*)

    let rec pr_ty ppf = function
      | TBot -> Format.fprintf ppf "\u{22A5}\u{209C}" (* \bot_t*)
      | TTop -> Format.fprintf ppf "\u{22A4}"         (* \top_t*)
      | TBase -> Format.fprintf ppf "t" 
      | TFun (v1, v2, eff) -> Format.fprintf ppf "@[(%a -%a-> %a)@]" pr_ty v1 pr_eff eff pr_ty v2
      | TTuple vs -> Format.pp_print_list ~pp_sep: (fun _ () -> Format.printf ";@ ") pr_ty ppf vs

    let rec join_ty v1 v2 = 
      let res = match v1, v2 with
        | TTop, v | v, TTop -> TTop
        | TBot, v | v, TBot -> v
        | TBase , TBase -> TBase
        | TFun (v1i, v1o, a1), TFun (v2i, v2o, a2) -> TFun (join_ty v1i v2i, join_ty v1o v2o, join_eff a1 a2)
        | TTuple vs1, TTuple vs2 -> List.combine vs1 vs2 |> List.map (fun (v1, v2) -> join_ty v1 v2) |> (fun vs' -> TTuple vs') 
        | _, _ -> raise (CFAError "Not supported")
      in
      (* Format.fprintf Format.std_formatter "\n@[<hv>v1: %a@ JOIN@ v2: %a = %a@]@." 
        pr_ty v1 pr_ty v2 pr_ty res; *)
      res

    let rec equal_ty v1 v2 = 
      match v1, v2 with
      | TBot, TBot | TTop, TTop | TBase, TBase -> true
      | TFun (v1i, v1o, _), TFun (v2i, v2o, _) -> equal_ty v1i v2i && equal_ty v1o v2o  
      | TTuple vs1, TTuple vs2 -> List.combine vs1 vs2 |> List.fold_left (fun res (v1, v2) -> res && (equal_ty v1 v2)) true 
      | _, _ -> false

    let rec leq_ty v1 v2 = 
      match v1, v2 with
      | TBot, _ -> true
      | _ , TTop -> true
      | TBase, TBase -> true
      | TFun (v1i, v1o, a1), TFun (v2i, v2o, a2) -> leq_ty v1i v2i && leq_ty v1o v2o && leq_eff a1 a2
      | TTuple vs1, TTuple vs2 -> List.combine vs1 vs2 |> List.fold_left (fun res (v1, v2) -> res && (leq_ty v1 v2)) true 
      | _, _ -> false

    let rec prop_ty (v1: ty_t) (v2: ty_t) : (ty_t * ty_t) = 
      match v1, v2 with
      | _, TBot -> v1, v1
      | _, TTop -> TTop, TTop  
      | TBase, TBase -> v1, v2
      (* | TBot, TFun (v2i, v2o, a2) -> *)
      (*    (TFun (v2i, TBot, EBot), TFun (v2i, v2o, a2)) *)
      | TFun (v1i, v1o, a1), TFun (v2i, v2o, a2) ->
         let v2i', v1i' = prop_ty v2i v1i in
         let v1o', v2o' = prop_ty v1o v2o in
         let a1', a2' = a1, join_eff a1 a2 in
         (TFun (v1i', v1o', a1'), TFun (v2i', v2o', a2'))
      | TTuple vs1, TTuple vs2 -> List.combine vs1 vs2 |> List.map (fun (v1, v2) -> prop_ty v1 v2) |> List.split
                                 |> (fun (vs1', vs2') -> TTuple vs1', TTuple vs2')
      | TBot, _ -> v1, v2
      | _ -> raise (CFAError (Format.asprintf "Invalid propagation operation (%a) PROP (%a)" pr_ty v1 pr_ty v2))

    (* Pretty printing *)
    let pr_t ppf = function
      | Bot -> Format.fprintf ppf "\u{22A5}"
      | Val (v, eff) -> Format.fprintf ppf "@[%a&%a@]" pr_ty v pr_eff eff 
      (* | Val (v, eff) -> Format.fprintf ppf "@[(v: %a,@ eff: %a)@]" pr_ty v pr_eff eff *)
      | Top -> Format.fprintf ppf "\u{22A4}" 

    let join va1 va2 = 
      let res = match va1, va2 with
        | Bot, va | va, Bot -> va
        | Top, va | va, Top -> Top
        | Val (v1, a1) , Val (v2, a2) ->  
           (* if not @@ equal_ty v1 v2 then raise (CFAError "Types not equal ")  *)
         (* else *) 
           Val (join_ty v1 v2, join_eff a1 a2)
      in 
      (* Format.fprintf Format.std_formatter "\n@[<hv>va1: %a@ JOIN@ va2: %a = %a@]@."  *)
      (*   pr_t va1 pr_t va2 pr_t res; *)
      res

    let leq va1 va2 =
      match va1, va2 with
      | Bot ,  _ -> true
      | _, Top -> true
      | Val (v1, a1), Val (v2, a2) -> leq_ty v1 v2 && leq_eff a1 a2
      | _, _ -> false 

    let is_bot = function
      | Bot -> true
      | Val (TBot, _) -> true
      | _ -> false

    let prop va1 va2 = 
      match va1, va2 with
      | Val (v, a), Bot -> 
         let v1', v2' = prop_ty v TBot in
         let a1', a2' = a, join_eff a EBot in
         Val (v1', a1'), Val (v2', a2') 
      | Val (v1, a1), Val (v2, a2) ->
         let v1', v2' = prop_ty v1 v2 in
         let a1', a2' = a1, join_eff a1 a2 in
         Val (v1', a1'), Val (v2', a2')
      | _, Top -> Top, Top
      |  _, _ -> va1, join va1 va2
  end

(* CFA Execution Map : Vars U Locations -> Fact.t *)
module CFAMap = 
  struct

    type t = Fact.t StringMap.t
    
    (* pretty printing *)
    let pr_mapping ppf (x, v) = 
      Format.fprintf ppf "@<10>%s \u{21A6}\t  %a" x Fact.pr_t v

    let pr_map ppf m = 
      let compare_key x1 x2 = 
        let x1int, x1, b1 = try (int_of_string x1), x1, true with Failure _ -> (0, x1, false) in
        let x2int, x2, b2 = try (int_of_string x2), x2, true with Failure _ -> (0, x2, false) in
        if b1 && b2 then Int.compare x1int x2int
        else if b1 && (not b2) then 1
        else if (not b1) && b2 then -1
        else String.compare x1 x2
      in
      StringMap.bindings m 
      |> List.sort (fun (x1, v1) (x2, v2) -> compare_key x1 x2)  
      |> Format.pp_print_list ~pp_sep: (fun _ () -> Format.printf ";@.") pr_mapping ppf 

    (* empty map *)
    let empty = StringMap.empty
    
    (* add mapping *)
    let add = StringMap.add

    (* lookup *)
    let find (m: t) (l : var) = 
      StringMap.find_opt l m |> Opt.get_or_else Fact.Bot
    (* Update mapping in an analysis map: m' = m[x |-> v'] *)
    let update l v' m = 
      StringMap.update l (fun opt_v -> Opt.get_or_else Fact.Bot opt_v |> (fun v -> Some (Fact.join v v'))) m 

    let join m m' = 
      StringMap.union (fun l v v'  -> Some (Fact.join v v')) m m'

    (* test: m <= m' *)
    let leq m m' = 
      StringMap.fold 
        (fun l va res -> res && (Fact.leq va (StringMap.find_opt l m' |> Opt.get_or_else Fact.Bot))) 
        m true
  end 

(* binary op types *)
let get_binop_type (bop: binop) = 
  match bop with
    Plus | Mult | Div | Mod | Modc | Minus
    | Eq | Ne 
    | Lt | Gt | Ge | Le 
    | And | Or -> (Fact.TBase, Fact.TBase, Fact.TBase)
    | Seq -> (Fact.TBase, Fact.TBase, Fact.TBase)
    | Cons | Null -> raise (CFAError "Cons and NULL binop not supported")

(* unary op type *)
let get_unop_type (uop: unop) =
  match uop with
    UMinus | Not -> (Fact.TBase, Fact.TBase)

(* evaluation environment and auxiliary operators *)
type env = Syntax.loc StringMap.t
let env_empty () = StringMap.empty
let lookup_env env x = StringMap.find x env
let extend_env env x v = StringMap.add x v env

let pr_env_mapping ppf (x, v) =
    Format.fprintf ppf "(%s |-> %s)" x v
let pr_env ppf env = 
  Format.pp_print_list ~pp_sep: (fun _ () -> Format.printf ";@ ") pr_env_mapping ppf (StringMap.bindings env) 

(* Flow function *)
let rec flow (env : env) (e : term) (m : CFAMap.t) : CFAMap.t = 
  let open Fact in
  let update_map = CFAMap.update in
  let find_map = CFAMap.find in
  let extract_v_eff ve = 
    match ve with
    | Bot -> TBot, EBot
    | Top -> raise (CFAError "Unxepectd value Top")
    | Val(v, eff) -> v, eff
  in
  match e with
    | Const (k, l) -> update_map l (Val (TBase, CertNot)) m
    | Var (x, l) -> 
       Format.fprintf Format.std_formatter "env_lookup %s@ Env:@[%a@]@." x pr_env env;
       let lx = lookup_env env x in
       let vex, ve = find_map m lx, find_map m l in
       let ve = match ve with Bot -> Val (TBot, CertNot) | _ -> ve in 
       let (vex', ve') = Fact.prop vex ve in
       update_map lx vex' m |> update_map l ve'
    | Rec (f_opt, (x, lx), e1, l) ->
       Format.fprintf Format.std_formatter "@[<2>Rec^%s: %a@]@." (loc e) (Printer.pr_exp true) e;
       let ve = find_map m l in
       Format.fprintf Format.std_formatter "Rec^%s \u{21A6} @[%a@]@." (loc e) Fact.pr_t ve;
       let v, eff = begin match ve with
                    | Bot -> TFun (TBot, TBot, EBot), CertNot 
                    | Val (TBot, eff) -> TFun (TBot, TBot, EBot), eff
                    | Val (TFun _ as vf, eff) -> vf, eff
                    | _ -> raise (CFAError "Unxepectd value")
                    end in       
       let env' = Option.map(fun (f, lf) -> extend_env env f lf) f_opt |> Opt.get_or_else env in
       let vx, vo, feff =  match v with 
         |TFun (v1, v2, feff) -> v1, v2, feff 
         | _ -> raise (CFAError "Fun shape expected") in 
       let m' = update_map lx (Val (vx, Fact.EBot)) m in 
       if vx = TBot then 
         let ve = Val (v, eff) in
         update_map l ve m'
         |> (fun m'' -> Option.map (fun (_, lf) -> update_map lf ve m'') f_opt |> Opt.get_or_else m'') 
       else 
         begin
           Format.fprintf Format.std_formatter "Rec^%s, type for %s: @[%a@]@." (loc e) x Fact.pr_ty vx;
           let env'' = extend_env env' x lx in
           let m'' = flow env'' e1 m' in           
           let vex = find_map m'' lx in
           let vx, _ = extract_v_eff vex in           
           let ve1 = find_map m'' (loc e1) in           
           let v1, eff1 =  extract_v_eff ve1 in
           let ver = Val (TFun (vx, v1, eff1), EBot) in
           Format.fprintf Format.std_formatter "Rec^%s, Prop oper: %a JOIN %a@." 
             l Fact.pr_t ver Fact.pr_t ve;  
           let ver', ve' = Fact.prop ver ve in
           Format.fprintf Format.std_formatter "Rec^%s, Prop oper res: ver' = %a, ve'= %a@." 
             l Fact.pr_t ver' Fact.pr_t ve';
           let vx', v1', eff' = match ver' with
             | Val (TFun (vx, vo, eff), _) -> vx, vo, eff 
             | _ -> raise (CFAError "Fun shape expected")
           in
           update_map lx (Val (vx', EBot)) m''
           |> update_map (loc e1) (Val (v1', eff'))
           |> update_map l ve'
           |> (fun m''' -> Option.map (fun (_, lf) ->
                            let vf = find_map m''' lf in
                            let ve', vf' = Fact.prop ve' vf in
                            update_map lf vf' m''') f_opt |> Opt.get_or_else m''') 
         end
    | App (e1, e2, l) ->
       Format.fprintf Format.std_formatter "@[<2>App^%s: %a@]@." (loc e) (Printer.pr_exp true) e;
       let m' = flow env e1 m in
       let m'' = flow env e2 m' in
       let ve1 = find_map m'' (loc e1) in
       if Fact.is_bot ve1 then m''
       else
         begin
           let v1, eff1 = begin match ve1 with
                          | Val (TFun _ as vf, eff) -> vf, eff
                          | _ -> raise (CFAError (Format.sprintf "Expected a function at loc %s" (loc e1)))
                          end in
           let ve2 = find_map m'' (loc e2) in
           let v2, eff2 = extract_v_eff ve2 in
           let ve = find_map m'' l in
           let v, eff = extract_v_eff ve in
           (* propagate args from call-sites to def-sites and results back from def-sites to call-sites *)
           let vec = Val (TFun (v2, v, EBot), EBot) in
           Format.fprintf Format.std_formatter "App^%s, Prop oper: %a JOIN %a@." 
             l Fact.pr_t ve1 Fact.pr_t vec;  
           let ve1', vec' = Fact.prop ve1 vec in       
           Format.fprintf Format.std_formatter "App^%s, Prop oper res: ve1' = %a, vec'= %a@." 
             l Fact.pr_t ve1' Fact.pr_t vec';
           let v2', v', eff' = begin match vec' with 
                          | Fact.(Val (TFun (v2', v', eff'), _)) -> v2', v', eff'
                          | _ -> raise (CFAError "Expected a function") 
                          end in
           Format.fprintf Format.std_formatter "App^%s[e1^%s] \u{21A6} @[%a@]@." l (loc e1) Fact.pr_t ve1';
           Format.fprintf Format.std_formatter "App^%s[e2^%s] \u{21A6} @[%a@]@." l (loc e2) Fact.pr_t ve2;
           Format.fprintf Format.std_formatter "App^%s \u{21A6} @[%a@]@." l Fact.pr_t Fact.(Val (v', eff'));

           update_map (loc e1) ve1' m''
           |> update_map (loc e2) (Val (v2', eff2))
           |> update_map l (Val(v', join_eff eff1 (join_eff eff2 eff')))
         end
    | TupleLst (es, l) ->
       let v, eff, m' = List.fold_left (fun (vs, eff, m) e ->
                            let m' = flow env e m in
                            let v, eff' = extract_v_eff (find_map m' (loc e)) in
                            (v::vs, join_eff eff eff', m')) ([], EBot, m) es
                        |> (fun (vs, eff, m') -> (TTuple (List.rev vs), eff, m')) in       
       update_map l (Val(v, eff)) m'
    | NonDet l -> 
       update_map l (Val(TBase, CertNot)) m
    | BinOp (bop, e1, e2, l) ->
       let m' = flow env e1 m in
       let m'' = flow env e2 m' in
       let v1, eff1 = extract_v_eff (find_map m'' (loc e1)) in
       let v2, eff2 = extract_v_eff (find_map m'' (loc e2)) in
       let bop_v1, bop_v2, bop_v = get_binop_type bop in
       update_map (loc e1) (Val (join_ty v1 bop_v1, eff1)) m''
       |> update_map (loc e2) (Val (join_ty v2 bop_v2, eff2))
       |> update_map l (Val (bop_v, join_eff eff1 eff2))
    | UnOp (uop, e1, l) ->
       let m' = flow env e1 m in
       let v1, eff1 = extract_v_eff (find_map m' (loc e1)) in
       let uop_v1, uop_v = get_unop_type uop in
       update_map (loc e1) (Val (join_ty v1 uop_v1, eff1)) m'
       |> update_map l (Val (uop_v, eff1))
    | Event (e, l) -> 
       let m' = flow env e m in
       update_map l (Val (TBase, ETop)) m'
    | Ite (e0, e1, e2, l) ->
       let m' = flow env e0 m in
       let m'' = flow env e1 m' in
       let m''' = flow env e2 m'' in
       let v0, eff0 = extract_v_eff (find_map m''' (loc e0)) in
       let v1, eff1 = extract_v_eff (find_map m''' (loc e1)) in
       let v2, eff2 = extract_v_eff (find_map m''' (loc e2)) in
       update_map (loc e0) (Val (join_ty v0 TBase, eff0)) m'''
       |> update_map (loc e1) (Val (v1, join_eff eff1 eff2))
       |> update_map (loc e2) (Val (v2, join_eff eff1 eff2))
       |> update_map l (Val (join_ty v1 v2, join_eff eff0 (join_eff eff1 eff2)))
    | PatMat (e, patlst, l) ->
       let m' = flow env e m in
       let v, eff = extract_v_eff (find_map m' (loc e)) in
       let v', eff', m' = 
         List.fold_left (fun (v', eff', m') (Case (e1, e2)) ->
             match e1 with
             | Const (c, l) -> 
                failwith "CFA support for PatMat not implemented"
             | Var (x, l) -> failwith "CFA support for not implemented"
             | TupleLst (es, l) -> failwith "CFA support for not implemented"
             | _ -> raise (CFAError "Pattern Matching: not supported patterns other than const/var/tuples of vars" ) 
           ) (v, eff, m') patlst in
       update_map l (Val (v', eff')) m'
    | Assert (e, _, l) -> 
       let m' = flow env e m in
       let v, eff = extract_v_eff (find_map m' (loc e)) in
       update_map l (Val (TBase, eff)) m'
                          
         
(* Fixpoint *)
let rec fix i env e m = 
  let open CFAMap in 
  Format.printf "@.Iteration %d [Start]@." i;
  let m' = flow env e m in
  Format.fprintf Format.std_formatter "\n\nAnalysis Map (end)@.@[<v>%a@]@." CFAMap.pr_map m';
  Format.printf "@.Iteration %d [End]@." i;
  if leq m' m then m
  else fix (i+1) env e m'

(* Polymorphic type for annotated terms *)
type 'a annterm = 
  | AnnConst of value * 'a                                       (* i (int constant) *)
  | AnnVar of var * 'a                                           (* x (variable) *)
  | AnnApp of 'a annterm * 'a annterm  * 'a                      (* t1 t2 (function application) *)
  | AnnUnOp of unop * 'a annterm  * 'a                           (* uop t (unary operator) *)
  | AnnBinOp of binop * 'a annterm * 'a annterm  * 'a            (* t1 bop t2 (binary infix operator) *)
  | AnnNonDet of  'a                                             (* To introduce non-determinism *)
  | AnnPatMat of 'a annterm * 'a annpatcase list  * 'a           (* match t1 with t2 -> t3 | ... *)
  | AnnIte of 'a annterm * 'a annterm * 'a annterm  * 'a         (* if t1 then t2 else t3 (conditional) *)
  | AnnRec of var option * var * 'a annterm  * 'a                (*lambda and recursive function*)
  | AnnEvent of 'a annterm  * 'a                                 (* event *)
  | AnnAssert of 'a annterm * pos  * 'a
  | AnnTupleLst of 'a annterm list   * 'a                        (* tuple list *)
and 'a annpatcase = 
  | AnnCase of 'a annterm * 'a annterm

(* Term annotated with analysis Fact.t *)
type fterm = Fact.t annterm

(* Return an expression's annotation *)
let ann = function
  | AnnConst (_, a) -> a                                     
  | AnnVar (_, a) -> a                                         
  | AnnApp (_, _, a) -> a
  | AnnUnOp (_, _, a) -> a
  | AnnBinOp (_, _, _, a) -> a
  | AnnNonDet a -> a
  | AnnPatMat (_, _ , a) -> a
  | AnnIte (_, _, _, a) -> a
  | AnnRec (_, _, _, a) -> a
  | AnnEvent (_, a) -> a
  | AnnAssert (_, _, a) -> a
  | AnnTupleLst (_, a) -> a

let rec pr_fterm ppf e =
  let open Fact in
  let open Printer in
  match e with
  | AnnConst (c, a) -> Format.fprintf ppf "@[(%a:%a)@]" pr_const c pr_t a
  | AnnVar (x, a) -> Format.fprintf ppf "@[(%s:%a)@]" x pr_t a
  | AnnNonDet a -> Format.fprintf ppf "@[(*:%a)@]" pr_t a
  | AnnUnOp (uop, ea, a) -> Format.fprintf ppf "@[<2>((%a@ %a):%a)@]" pr_unop uop pr_fterm ea pr_t a
  | AnnBinOp (bop, ea1, ea2, a) -> 
     Format.fprintf ppf "@[<2>((%a@ %a@ %a):%a)@]" pr_fterm ea1 pr_op bop pr_fterm ea2 pr_t a
  | AnnApp (ea1, ea2, a) -> 
     Format.fprintf ppf "@[<2>((%a@ %a):%a)@]" pr_fterm ea1 pr_fterm ea2 pr_t a
  | AnnPatMat (ea, patlst, a) -> 
     Format.fprintf ppf "@[<2>((match@ %a with@ %a):%a)@]" pr_fterm ea pr_fpatlst patlst pr_t a 
  | AnnIte (ea0, ea1, ea2, a) -> 
     Format.fprintf ppf "Ite @[<2>((%a@ ?@ %a@ :@ %a):%a)@]"
       pr_fterm ea0 pr_fterm ea1 pr_fterm ea2 pr_t a
  | AnnRec (None, x, ea, a) ->
     Format.fprintf ppf "@[<2>((lambda@ %s.%a):%a)@]" x pr_fterm ea pr_t a
  | AnnRec (Some f, x, ea, a) ->
     Format.fprintf ppf "@[<2>((mu@ %s@ %s.%a):%a)@]" f x pr_fterm ea pr_t a
  | AnnEvent (ea, a) -> Format.fprintf ppf "@[<2>((ev@ %a):%a)@]" pr_fterm ea pr_t a
  | AnnAssert (ea, _, a) -> Format.fprintf ppf "@[<2>((assert %a):%a)@]" pr_fterm ea pr_t a
  | AnnTupleLst (eas, a) -> 
     let pr_ftermlst ppf eas = Format.pp_print_list ~pp_sep:(fun _ () -> Format.printf ",@ ") pr_fterm ppf eas in
     Format.fprintf ppf "@[<2>((Tup (%a)):%a)@]" pr_ftermlst eas pr_t a
and pr_fpatlst ppf patlst =
  Format.pp_print_list ~pp_sep:(fun _ () -> Format.printf "@ |@ ") pr_fpat ppf patlst
and pr_fpat ppf (AnnCase (ea1, ea2)) =
  Format.fprintf ppf "@[<2>%a@ ->@ %a@]" pr_fterm ea1 pr_fterm ea2

(* Convert Exp tree to Fact.t AnnotatedTree *)
let rec fterm_of_term e m = 
  let get_fact m l = CFAMap.find m l in
  match e with
  | Const (c, l) -> AnnConst (c, get_fact m l)
  | Var (x, l) -> AnnVar (x, get_fact m l)
  | NonDet l -> AnnNonDet (get_fact m l)
  | UnOp (uop, e, l) -> AnnUnOp (uop, fterm_of_term e m, get_fact m l)
  | BinOp (bop, e1, e2, l) -> AnnBinOp (bop, fterm_of_term e1 m, fterm_of_term e2 m, get_fact m l)
  | App (e1, e2, l) -> AnnApp (fterm_of_term e1 m, fterm_of_term e2 m, get_fact m l)
  | Rec (fopt, (x, _), e, l) -> AnnRec (Option.map fst fopt, x, (fterm_of_term e m), get_fact m l)
  | Ite (e0, e1, e2, l) -> AnnIte (fterm_of_term e0 m, fterm_of_term e1 m, fterm_of_term e2 m, get_fact m l)
  | PatMat (e, patlst, l) -> AnnPatMat (fterm_of_term e m, 
                                       List.map (fun (Case (e1, e2)) -> AnnCase (fterm_of_term e1 m, fterm_of_term e2 m)) patlst,
                                       get_fact m l)
  | Event (e, l) -> AnnEvent (fterm_of_term e m, get_fact m l)
  | Assert (e, pos, l) -> AnnAssert (fterm_of_term e m, pos, get_fact m l)
  | TupleLst (es, l) -> AnnTupleLst (List.map (fun e -> fterm_of_term e m) es, get_fact m l) 
                                           
(* Control Flow Analysis for marking program with Effect Annotations *)
let annotate_eff e =
  let fv_e = fv e in
  let m0, env0 = StringSet.fold (fun prefx (m, env) ->
                     let lprefx = prefx in
                     let m' = CFAMap.add lprefx (Fact.Val (TBase, EBot)) m in
                     let env' = extend_env env prefx lprefx in
                     (m', env')) fv_e (CFAMap.empty, env_empty ()) in
  Format.fprintf Format.std_formatter "\nMap0:@.@[%a@]@.@." CFAMap.pr_map m0;
  let m = fix 1 env0 e m0 in
  Format.printf "\nAnalysis Ended@.@.";
  Format.fprintf Format.std_formatter "Prog :@[%a@]@.@." (Printer.pr_exp true) e;
  fterm_of_term e m
