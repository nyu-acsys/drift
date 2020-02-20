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

let update_fun p q = match p with
| None -> Some [q]
| Some ls -> Some (q::ls)

let change_key prek k m = 
  let v = VarDefMap.find_opt prek m in
  match v with
  | None -> m
  | Some vl -> m |> VarDefMap.add k vl |> VarDefMap.remove prek

%}
/* declarations */

%token <string> IDENT
%token <int> INTCONST
%token <bool> BOOLCONST
%token LPAREN RPAREN
%token PLUS MINUS DIV TIMES MOD
%token EQ NE LE GE LT GT
%token EOF
%token AND OR
%token INT BOOL
%token ARROW
%token SEMI COLON
%token IF ELSE THEN FUN LET REC IN ASSERT

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

%start main
%type <Syntax.term> main
%% /* rules */

main:
| seq_term EOF { $1 }
| error { 
  let loc = mklocation $startpos $endpos in
  fail loc "Syntax error" }
;

type_del:
| INT { Integer 0 }
| BOOL { Boolean true }
;

param_list:
| IDENT param_list { $1 :: $2 }
| LPAREN IDENT COLON type_del RPAREN param_list { top_var := VarDefMap.update fun_name (fun a -> update_fun a ($2, $4)) !top_var; $2 :: $6 }
| IDENT { [$1] }
| LPAREN IDENT COLON type_del RPAREN { top_var := VarDefMap.update fun_name (fun a -> update_fun a ($2, $4)) !top_var; [$2] }
;

ident_list:
| IDENT { [$1] }
| IDENT param_list {
  top_var := change_key fun_name $1 !top_var;
  $1 :: $2 
}
;

basic_term: /*term@7 := int | bool | var | (term)*/
| INTCONST { Const (Integer $1, "") }
| BOOLCONST { Const (Boolean $1, "") }
| IDENT { Var ($1, "") }
| LPAREN seq_term RPAREN { $2 } /*Parentheses*/
| LPAREN RPAREN { Void("") }
;

app_term: /* term@6 := term@6 term@7 | term@7 */
| basic_term { $1 }
| app_term basic_term { App ($1, $2, "") }
| ASSERT LPAREN seq_term RPAREN { Ite ($3, Const (Boolean true, ""), Const (Boolean false, ""), "")}
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

comp_op:
| EQ { Eq }
| NE { Ne }
| LE { Le }
| GE { Ge }
| LT { Lt }
| GT { Gt }
;

comp_term: /* term@3 := term@3 cop term@4 */
| add_term { $1 }
| comp_term comp_op add_term { BinOp ($2, $1, $3, "") }
;

bool_op:
| AND { And }
| OR { Or }
;

bool_term: /* term@2 := term@2 bop term@3 */
| comp_term { $1 }
| comp_term bool_op bool_term { BinOp ($2, $1, $3, "") }
;

%inline if_term_:
| bool_term { $1 }
| IF seq_term THEN term ELSE term { Ite ($2, $4, $6, "") }
;

lambda_term:
| FUN IDENT ARROW seq_term { mk_lambda $2 $4 }
;

let_in_term:
| LET REC LPAREN IDENT COLON type_del RPAREN EQ seq_term IN seq_term {
  let fn = mk_lambdas [] $9 in
  mk_let_rec_in $4 fn $11
}
| LET LPAREN IDENT COLON type_del RPAREN EQ seq_term IN seq_term {
  let fn = mk_lambdas [] $8 in
  mk_let_in $3 fn $10
}
| LET REC ident_list EQ seq_term IN seq_term {
  let fn = mk_lambdas (List.tl $3) $5 in
  mk_let_rec_in (List.hd $3) fn $7
}
| LET ident_list EQ seq_term IN seq_term {
  let fn = mk_lambdas (List.tl $2) $4 in
  mk_let_in (List.hd $2) fn $6
}
;

term:
| if_term_ { $1 }
| lambda_term { $1 }
| let_in_term { $1 }
;

seq_term:
| term %prec below_SEMI { $1 }
| term SEMI { $1 }
| term SEMI seq_term { mk_let_in "_" $1 $3 }
;

/*
let_val:
| LET ident_list EQ term {

}
| LET REC ident_list EQ term {

}
;
*/
