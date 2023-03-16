(** Grammar rules for lexer of MiniML *)
{
open Util
open DriftSyntax
open DriftGrammar
open Lexing

let keyword_table = Hashtbl.create 32
let _ =
  List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
    ([("begin", BEGIN);
      ("else", ELSE);
      ("end", END);
      ("false", BOOLCONST false);
      ("assert", ASSERT);
      ("fun", FUN);
      ("if", IF);
      ("in", IN);
      ("match", MATCH);
      ("with", WITH);
      ("let", LET);
      ("mod", MOD);
      ("rec", REC);
      ("then", THEN);
      ("ev", EV);
      ("true", BOOLCONST true)])

let lexical_error lexbuf msg =
  let pos = lexeme_start_p lexbuf in 
  let pos_line = pos.pos_lnum in
  let pos_col = pos.pos_cnum - pos.pos_bol in
  let pos' = { pl = pos_line; pc = pos_col } in
  fail pos' "Syntax error"
}

let white_space = [' ']*
let basic_type = "int" | "bool" | "unit"
let types = "int" | "bool" | "unit"
let digitchar = ['0'-'9']
let idchar = ['A'-'Z''a'-'z''_']
let primechar = [''']
let minus = ['-']
let ident = idchar(idchar | digitchar)*('.'idchar)?(idchar | digitchar | primechar)* 
let digits = digitchar+
let bool_str = "true" | "false" | "either"
let bin_op_str =  ['>''<''='] | "<=" | ">=" | "<>" 
let arth_op_str = ['+''-''*''/'] | "mod" | "&&" | "||" 
let empty_list = "[" white_space "]"

rule token = parse
  [' ' '\t'] { token lexbuf }
| '\n' { Lexing.new_line lexbuf; token lexbuf }
| "(*" { comments 0 lexbuf }
| "(*-:{"("v"as v)":"white_space("unit"as d)white_space"|"white_space("unit" as t)white_space"}*)"
{
  let vk = init_ref ("cur_"^ Char.escaped v) (string_to_type d) t "=" t in 
  PRE (vk)
}
| "(*-:{"("v"as v)":"white_space(("bool"|"int")as d)white_space"|"white_space(bool_str as t)white_space"}*)"
{ 
  let vk = init_ref ("cur_"^ Char.escaped v) (string_to_type d) t "=" t in 
  PRE (vk) 
}
| "(*-:{"("v"as v)":"white_space(("bool"|"int")as d)white_space"|"white_space("v" as l)white_space(bin_op_str as bop)white_space((ident|digits) as r)white_space"}*)"
{ 
  let r' = try string_of_int (int_of_string r)
    with _ -> "pref"^r in
  let vk = init_ref ("cur_"^ Char.escaped v) (string_to_type d) ("cur_"^Char.escaped l) bop r' in 
  PRE (vk) 
}
| ":"  { COLON }
| ","  { COMMA }
| ";"  { SEMI }
| "->" { ARROW }
| "<=" { LE }
| ">=" { GE }
| "&&" { AND }
| "||" { OR }
| "|" { BAR }
| "=" { EQ }
| "<>" { NE }
| "<" { LT }
| ">" { GT }
| "::" { CONS }
| "[" { LSQBR }
| "]" { RSQBR }
| "[|" { LARYBR }
| "|]" { RARYBR }
| '+' { PLUS }
| '-' { MINUS }
| '/' { DIV }
| types as t { TYPE (t) }
| '*' { TIMES }
| '(' { LPAREN }
| ')' { RPAREN }
| ident as kw
    { try
      Hashtbl.find keyword_table kw
    with Not_found ->
      IDENT (kw)
    }
| empty_list { EMPTYLST }
| digits as num { INTCONST (int_of_string num) }
| eof { EOF }
| _ { lexical_error lexbuf None }

and comments level = parse
| "*)" { if level = 0 then token lexbuf
         else comments (level - 1) lexbuf
       }
| "(*" { comments (level + 1) lexbuf }
| '\n' { Lexing.new_line lexbuf; comments (level) lexbuf }
| _ { comments level lexbuf }
| eof { token lexbuf }
