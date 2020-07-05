%{
(* header *)
(** {0} Grammar rules for parser of Mini-OCaml *)

open Util
open Syntax
open Lexing

let mklocation s e = {
  pl = s.pos_lnum;
  pc = s.pos_cnum - s.pos_bol;
}

let final_call_name = "main"

let universe_name = "umain"

%}
/* declarations */
/* let final_call_name = function
  | Var (x, _) -> x = "main"
  | _ -> false */

%token <string> IDENT
%token <int> INTCONST
%token <bool> BOOLCONST
%token EMPTYLST
%token LPAREN RPAREN
%token PLUS MINUS DIV TIMES MOD
%token EQ NE LE GE LT GT
%token EOF
%token AND OR
%token ARROW CONS BAR 
%token SEMI COLON COMMA LSQBR RSQBR LARYBR RARYBR
%token IF ELSE THEN FUN LET REC IN ASSERT MATCH WITH
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
%left BAR

%start main
%type <Syntax.term> main
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
| IDENT param_list { $1 :: $2 }
| LPAREN IDENT COLON TYPE RPAREN param_list { $2 :: $6 }
| LPAREN IDENT COLON TYPE RPAREN fun_type { [$2] }
| IDENT fun_type { [$1] }
;

param_ref_list:
| LPAREN IDENT COLON TYPE PRE RPAREN param_ref_list { pre_vars := VarDefMap.add ("pref"^$2) $5 !pre_vars; $2 :: $7 }
| LPAREN IDENT COLON TYPE PRE RPAREN fun_type { pre_vars := VarDefMap.add ("pref"^$2) $5 !pre_vars; [$2] }
| LPAREN IDENT PRE RPAREN param_ref_list { pre_vars := VarDefMap.add ("pref"^$2) $3 !pre_vars; $2 :: $5 }
| LPAREN IDENT PRE RPAREN fun_type { pre_vars := VarDefMap.add ("pref"^$2) $3 !pre_vars; [$2] }
;

ident_list:
| IDENT COLON TYPE param_list { $1 :: $4 }
| IDENT param_list { $1 :: $2 }
;

ident_ref_list:
| IDENT param_ref_list { $1 :: $2 }
;

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
| INTCONST { thresholdsSet := ThresholdsSetType.add $1 !thresholdsSet; Const (Integer $1, "") }
| BOOLCONST { Const (Boolean $1, "") }
| EMPTYLST { Const (IntList [], "") }
| LSQBR int_list RSQBR { Const (IntList $2, "") }
| LARYBR int_ary RARYBR { Const (IntList [], "") } /*TODO: Implement this*/
| IDENT { Var ($1, "") }
| LPAREN seq_term RPAREN { $2 } /*Parentheses*/
| LPAREN RPAREN { Const (Unit (), "") }
;

app_term: /* term@6 := term@6 term@7 | term@7 */
| basic_term { $1 }
| app_term basic_term { App ($1, $2, "") }
| ASSERT LPAREN seq_term RPAREN { 
  let loc = Some (mklocation $startpos $endpos) |> construct_asst in
  Ite ($3, Const (Unit (), ""), Const (Unit (), ""), "", loc)}
;

mult_op:
| TIMES { Mult }
| DIV { Div }
| MOD { Mod }
;

mult_term: /* term@5 := term@5 mop term@6 */
| app_term { $1 }
| mult_term mult_op app_term { BinOp ($2, $1, $3, "") }
;

add_op:
| PLUS { Plus }
| MINUS { Minus }
;

add_term: /* term@4 := term@4 aop term@5 */
| mult_term { $1 }
| add_term add_op mult_term { BinOp ($2, $1, $3, "") }
;

cons_op:
| CONS { Cons }
;

cons_term:
| add_term { $1 }
| add_term cons_op cons_term { BinOp ($2, $1, $3, "") }
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
| comp_term comp_op cons_term { BinOp ($2, $1, $3, "") }
;

bool_op:
| AND { And }
| OR { Or }
;

bool_term: /* term@2 := term@2 bop term@3 */
| comp_term { $1 }
| comp_term bool_op bool_term { BinOp ($2, $1, $3, "") }
;

tuple_term2:
| bool_term COMMA bool_term { $1 :: [$3] }
| bool_term COMMA tuple_term2 { $1 :: $3 } 
;

tuple_term:
| bool_term { $1 }
| tuple_term2 { TupleLst ($1, "") }
;

%inline if_term_:
| tuple_term { $1 }
| IF seq_term THEN term ELSE term { 
  let loc = None |> construct_asst in
  Ite ($2, $4, $6, "", loc) 
}
;

lambda_term:
| FUN IDENT ARROW seq_term { mk_lambda $2 $4 }
;

let_in_term:
| LET REC ident_list EQ seq_term IN seq_term {
  let fn = mk_lambdas (List.tl $3) $5 in
  mk_let_rec_in (List.hd $3) fn $7
}
| LET ident_list EQ seq_term IN seq_term {
  let fn = mk_lambdas (List.tl $2) $4 in
  mk_let_in (List.hd $2) fn $6
}
| LET pattern EQ seq_term IN seq_term {
  mk_pattern_let_in $2 $4 $6
}
;

let_val:
| LET REC ident_list EQ seq_term {
  let fn = mk_lambdas (List.tl $3) $5 in
  1, (List.hd $3), fn, []
}
| LET ident_list EQ seq_term {
  let fn = mk_lambdas (List.tl $2) $4 in
  0, (List.hd $2), fn, []
}
| LET ident_ref_list EQ seq_term {
  let fn = mk_lambdas (List.tl $2) $4 in
  0, (List.hd $2), fn, (List.tl $2)
}
| LET REC ident_ref_list EQ seq_term {
  let fn = mk_lambdas (List.tl $3) $5 in
  1, (List.hd $3), fn, (List.tl $3)
}
;

/*
let main (x(*-: {v: int | true}*)) = x
<=> let main x = x in main prefx
*/
let_vals:
| let_val {
  let rc, x, def, lst = $1 in
  if rc = 0 then
    if x = final_call_name then
      mk_let_main x def lst
    else
      def
  else
    if x = final_call_name then
      mk_let_main x def lst
    else
      def
}
| LET pattern EQ seq_term {
  $4
}
| LET pattern EQ seq_term let_vals {
  mk_pattern_let_in $2 $4 $5
}
| LET REC pattern EQ seq_term {
  $5
}
| let_val let_vals {
  let rc, x, def, lst = $1 in
  if x = final_call_name && lst <> [] then
    mk_let_main x def lst
  else if rc = 0 then
    mk_let_in x def $2
  else 
    mk_let_rec_in x def $2
}
;

pattern_term: /*pattern_term := int | bool | [] | var | (pattern_term)*/
| INTCONST { thresholdsSet := ThresholdsSetType.add $1 !thresholdsSet; Const (Integer $1, "") }
| BOOLCONST { Const (Boolean $1, "") }
| EMPTYLST { Const (IntList [], "") }
| IDENT { Var ($1, "") }
| IDENT COLON TYPE { Var ($1, "") }
| LPAREN tuple_pattern RPAREN { TupleLst ($2, "") }
;

tuple_pattern:
| pattern_term COMMA pattern_term { $1 :: [$3] }
| pattern_term COMMA tuple_pattern { $1 :: $3 }
;

pattern:
| pattern_term { $1 }
| LPAREN pattern RPAREN { $2 } /*Parentheses*/
| pattern cons_op pattern_term { BinOp ($2, $1, $3, "") }
;

pattern_matching:
| pattern ARROW seq_term { mk_pattern_case $1 $3 }
;

pattern_matchings:
| pattern_matching %prec below_BAR { [$1] }
| pattern_matching BAR pattern_matchings { $1 :: $3 }
;

match_term:
| MATCH seq_term WITH pattern_matchings { PatMat ($2, $4, "") }
| MATCH seq_term WITH BAR pattern_matchings { PatMat ($2, $5, "") }
;

term:
| if_term_ { $1 }
| lambda_term { $1 }
| let_in_term { $1 }
| match_term { $1 }
;

seq_term:
| term %prec below_SEMI { $1 }
| term SEMI seq_term { mk_let_in "_" $1 $3 }
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