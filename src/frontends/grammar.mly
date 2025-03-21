%{
(* header *)
(** {0} Grammar rules for parser of Mini-OCaml *)

(* open Util *)
open Syntax
open Lexing

let mklocation s _ = {
  pl = s.pos_lnum;
  pc = s.pos_cnum - s.pos_bol;
}

let final_call_name = "main"

let universe_name = "umain"

let convert_var_name_apron_not_support s = 
  if String.contains s '\'' then
    let lst = String.split_on_char '\'' s in
    let lst' = List.fold_right (fun s rlst -> 
      if String.length s = 0 then List.append rlst ["_pm"]
      else List.append rlst [s; "_pm_"]) lst [] in
    let restr = 
      let tempstr = String.concat "" (List.rev lst') in
      String.sub tempstr 4 (String.length tempstr - 4)
    in
    restr
  else s

%}
/* declarations */
/* let final_call_name = function
  | MlVar x -> x = "main"
  | _ -> false */

%token <string> IDENT
%token <int> INTCONST
%token <bool> BOOLCONST
%token EMPTYLST
%token LPAREN RPAREN
%token PLUS MINUS DIV TIMES MOD
%token EQ NE LE GE LT GT
%token EOF NONDET
%token AND OR
%token ARROW CONS BAR 
%token SEMI COLON COMMA LSQBR RSQBR LARYBR RARYBR
%token IF ELSE THEN FUN LET REC IN ASSERT MATCH WITH EV
%token BEGIN END        
%token TYPE
%token <Syntax.pre_exp> PRE

/* 
This block comment is Copyright (©) 1996-present, Institut National de Recherche en Informatique et en Automatique. 

Precedences and associativities.

Tokens and rules have precedences.  A reduce/reduce conflict is resolved
in favor of the first rule (in source file order).  A shift/reduce conflict
is resolved by comparing the precedence and associativity of the token to
be shifted with those of the rule to be reduced.

By default, a rule has the precedence of its rightmost terminal (if any).

When there is a shift/reduce conflict between a rule and a token that
have the same precedence, it is resolved using the associativity:
if the token is left-associative, the parser will reduce; if
right-associative, the parser will shift; if non-associative,
the parser will declare a syntax error.

We will only use associativities with operators of the kind  x * x -> x
for example, in the rules of the form    expr: expr BINOP expr
in all other cases, we define two precedences if needed to resolve
conflicts.

The precedences must be listed from low to high.
*/
%nonassoc below_SEMI
%nonassoc SEMI
%nonassoc below_BAR
%nonassoc THEN
%nonassoc ELSE
%left BAR

%start main
%type <Syntax.mlterm> main
%% /* rules */

main:
| seq_term EOF { $1 }
| let_vals EOF { $1 }
| error { 
  let loc = mklocation $startpos $endpos in
  fail loc "Syntax error" }
;

fun_type:
| {}
| COLON TYPE {}
;

param_list:
| basic_pattern param_list_opt { $1 :: $2 }
;

param_list_opt:
| param_list { $1 }
| fun_type { [] }
  
int_list:
| INTCONST { [$1] }
| INTCONST SEMI { [$1] }
| INTCONST SEMI int_list { $1 :: $3 }
;

int_ary:
| INTCONST { [$1] }
| INTCONST SEMI { [$1] }
| INTCONST SEMI int_ary { $1 :: $3 }
;

basic_term: /*term@7 := int | bool | [] | var | (term)*/
| const_term { $1 }
| EMPTYLST { SemActions.mk_const (IntList []) }
| LSQBR int_list RSQBR { SemActions.mk_const (IntList $2) }
| LARYBR int_ary RARYBR { SemActions.mk_const (IntList []) } /*TODO: Implement this*/
| IDENT { 
  let res_str = convert_var_name_apron_not_support $1 in 
  SemActions.mk_var res_str }
| LPAREN seq_term RPAREN { $2 } /*Parentheses*/
| BEGIN seq_term END { $2 } /*begin/end blocks*/
;

const_term:
| INTCONST { if abs $1 < 1000 then thresholdsSet := ThresholdsSetType.add $1 !thresholdsSet; SemActions.mk_const (Integer $1) }
| BOOLCONST { SemActions.mk_const (Boolean $1) }
| LPAREN RPAREN { SemActions.mk_const (UnitLit) }
;
  
app_term: /* term@6 := term@6 term@7 | term@7 */
| basic_term { $1 }
| app_term basic_term { SemActions.mk_app $1 $2 }
| ASSERT basic_term { 
  (* let loc = mklocation $startpos $endpos) |> construct_asst in
  Ite ($2, Const (UnitLit, ""), Const (UnitLit, ""), "", loc) *)
  let loc = mklocation $startpos $endpos in 
  SemActions.mk_assert $2 loc }
;

unary_term:
| app_term { $1 }
| MINUS unary_term {
  match $2 with
  | MlConst (Integer i) ->
      if abs i < 1000 then thresholdsSet := ThresholdsSetType.add (-i) !thresholdsSet;
      SemActions.mk_const (Integer (-i))
  | _ -> SemActions.mk_unop (UMinus, $2) }
;

mult_op:
| TIMES { Mult }
| DIV { Div }
| MOD { Mod }
;

mult_term: /* term@5 := term@5 mop term@6 */
| unary_term { $1 }
| mult_term mult_op unary_term { SemActions.mk_binop ($2, $1, $3) }
;

add_op:
| PLUS { Plus }
| MINUS { Minus }
;

add_term: /* term@4 := term@4 aop term@5 */
| mult_term { $1 }
| add_term add_op mult_term { SemActions.mk_binop ($2, $1, $3) }
;

cons_op:
| CONS { Cons }
;

cons_term:
| add_term { $1 }
| add_term cons_op cons_term { SemActions.mk_binop ($2, $1, $3) }
;

comp_op:
| EQ { Eq }
| NE { Ne }
| LE { Le }
| GE { Ge }
| LT { Lt }
| GT { Gt }
;

comp_term: /* term@3 := term@3 cop term@4 */
| cons_term { $1 }
| comp_term comp_op cons_term { SemActions.mk_binop ($2, $1, $3) }
;

bool_op:
| AND { And }
| OR { Or }
;

bool_term: /* term@2 := term@2 bop term@3 */
| NONDET { SemActions.mk_nondet ()}
| NONDET bool_op bool_term { SemActions.mk_binop ($2, (SemActions.mk_nondet ()), $3) }
| comp_term { $1 }
| comp_term bool_op bool_term { SemActions.mk_binop ($2, $1, $3) }
;

tuple_term2:
| bool_term COMMA bool_term { $1 :: [$3] }
| bool_term COMMA tuple_term2 { $1 :: $3 } 
;

tuple_term:
| bool_term { $1 }
| tuple_term2 { SemActions.mk_tuple $1 }
;

%inline if_term_:
| tuple_term { $1 }
| IF seq_term THEN term {
  let else_term = SemActions.mk_const UnitLit in
  SemActions.mk_ite ($2, $4, else_term) 
}
| IF seq_term THEN term ELSE term { 
  SemActions.mk_ite ($2, $4, $6) 
}
;

lambda_term:
| FUN basic_pattern ARROW seq_term { SemActions.mk_lambda None [$2] $4 }
;

let_in_term:
| LET REC IDENT param_list_opt EQ seq_term IN seq_term {
  let res_str = convert_var_name_apron_not_support $3 in
  SemActions.mk_letrec_in (MlVar res_str) $4 $6 $8
}
| LET param_list EQ seq_term IN seq_term {
  SemActions.mk_let_in (List.hd $2) (List.tl $2) $4 $6
}
;

ev_term: 
| EV term { SemActions.mk_event $2 }
;

term:
| if_term_ { $1 }
| lambda_term { $1 }
| let_in_term { $1 }
| match_term { $1 }
| ev_term { $1 }
;

seq_term:
| term %prec below_SEMI { $1 }
| term SEMI seq_term { SemActions.mk_binop (Seq, $1, $3)  }
;

  /* let fn = mk_lambdas $4 $6 in */
/* true, Var (res_str, ""), fn, $4 */
/* let fn = mk_lambdas (List.tl $2) $4 in */
  /* false, (List.hd $2), fn, (List.tl $2) */
  
let_val:
| LET REC IDENT param_list_opt EQ seq_term {
  let res_str = convert_var_name_apron_not_support $3 in
  SemActions.mk_gdef (MlVar res_str) (Some res_str) $4 $6
}
| LET param_list EQ seq_term {
  SemActions.mk_gdef (List.hd $2) None (List.tl $2) $4
}
;

/*
let main (x(*-: {v: int | true}*)) = x
<=> let main x = x in main prefx
*/

/* let rc, p, def, lst = $1 in */
  /* match p, $2 with */
  /* | Var (("main" | "_"), _), Const (UnitLit, "") -> */
  /*     mk_let_main "main" def lst */
  /* | Var (x, _), _ when rc ->  */
  /*     mk_let_rec_in x def $2 */
  /* | _ -> */
  /*     mk_let_in p def $2 */

let_vals:
| let_val let_vals {
  let n, e = $1 in
  match n, $2 with
  | ("main" | "_"), MlConst UnitLit -> SemActions.mk_gdef_main e 	    
  | _ -> SemActions.mk_gdefs $1 $2
}
| /* empty */ { SemActions.mk_const UnitLit }   
;

basic_pattern:
| INTCONST { 
    if abs $1 < 1000 then thresholdsSet := ThresholdsSetType.add $1 !thresholdsSet; 
    SemActions.mk_const (Integer $1) }
| MINUS INTCONST { 
    if abs $2 < 1000 then thresholdsSet := ThresholdsSetType.add (-$2) !thresholdsSet; 
    SemActions.mk_const (Integer (-$2)) }
| BOOLCONST { SemActions.mk_const (Boolean $1) }
| EMPTYLST { SemActions.mk_const (IntList []) }
| IDENT { 
  let res_str = convert_var_name_apron_not_support $1 in 
  SemActions.mk_var (res_str) }
| LPAREN pattern RPAREN { $2 }
;
    
pattern:
| basic_pattern { $1 }
| basic_pattern COLON TYPE PRE {
  match $1 with
  | MlVar x ->
     pre_vars := VarDefMap.add ("pref" ^ x) $4 !pre_vars; $1
  | _ ->
     let loc = mklocation (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 4) in
     fail loc "Syntax error" 
  }
| basic_pattern PRE {
  match $1 with
  | MlVar x ->
     pre_vars := VarDefMap.add ("pref" ^ x) $2 !pre_vars; $1
  | _ ->
     let loc =  mklocation (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 2) in
     fail loc "Syntax error" 
  }
| basic_pattern COLON TYPE { $1 }
| tuple_pattern { SemActions.mk_tuple $1 }
| basic_pattern cons_op pattern { SemActions.mk_binop ($2, $1, $3) }
;
  
tuple_pattern:
| basic_pattern COMMA basic_pattern { $1 :: [$3] }
| basic_pattern COMMA tuple_pattern { $1 :: $3 }
;
  
pattern_matching:
| pattern ARROW seq_term { SemActions.mk_patcase $1 $3 }
;

pattern_matchings:
| pattern_matching %prec below_BAR { [$1] }
| pattern_matching BAR pattern_matchings { $1 :: $3 }
;

match_term:
| MATCH seq_term WITH pattern_matchings { SemActions.mk_patmat ($2, $4) }
| MATCH seq_term WITH BAR pattern_matchings { SemActions.mk_patmat ($2, $5) }
;



/*
let main (x(*-: {v: int | true}*)) =
  x

let _ = main 3
let _ = main 10

<=>

let main x = x in
let _ = main 3 in
let _ = main 10 in
main top_x

or
let main x = x in
let umain = main top_x in
let _ = main 3 in
let _ = main 10 in
umain
*/
