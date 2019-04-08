%{
(* header *)
(** {0} Grammar rules for parser of Mini-OCaml *)

open Util
open Syntax
open Lexing

%}
/* declarations */

%token <string> IDENT
%token <int> INTVAL
%token <bool> BOOLVAL
%token LPAREN RPAREN
%token PLUS MINUS DIV TIMES MOD
%token EQ NE LE GE LT GT
%token AND OR ARROW
%token IF ELSE THEN FUN LET REC IN
%token WHILE
%token EOF

%start main
%type <Syntax.term> main
%% /* rules */

main:
| term { $1 }
;

ident_list:
| IDENT ident_list { $1 :: $2 }
| IDENT { [$1] }
;

(*TODO: Add remain rules*)

