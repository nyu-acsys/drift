(** Grammar rules for lexer of MiniML *)
{
open Util
open Syntax
open Grammar
open Lexing

let keyword_table = Hashtbl.create 32
let _ =
  List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
    ([("else", ELSE);
      ("false", BOOLCONST false);
      ("assert", ASSERT);
      ("fun", FUN);
      ("if", IF);
      ("in", IN);
      ("let", LET);
      ("mod", MOD);
      ("rec", REC);
      ("then", THEN);
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
let rm_type_sig = ([^')''(''=']|'('[^')''(''=''*']+')')* 
let types = "int" rm_type_sig | "bool" rm_type_sig  | "unit" rm_type_sig 
let digitchar = ['0'-'9']
let idchar = ['A'-'Z''a'-'z''_']
let primechar = [''']
let minus = ['-']
let ident = idchar(idchar | digitchar)*('.'idchar)?(idchar | digitchar | primechar)* 
let digits = minus? digitchar+
let bool_str = "true" | "false" | "either"
let bin_op_str =  ['>''<''='] | "<=" | ">=" | "<>" 
let arth_op_str = ['+''-''*''/'] | "mod" | "&&" | "||" 

rule token = parse
  [' ' '\t'] { token lexbuf }
| '\n' { Lexing.new_line lexbuf; token lexbuf }
| "(*" { comments 0 lexbuf }
| "(*-:{"("v"as v)":"white_space("Unit"as d)white_space"|"white_space("unit" as t)white_space"}*)"  
{
  let vk = init_ref ("cur_"^ Char.escaped v) (string_to_type d) t "=" t in 
  PRE (vk)
}
| "(*-:{"("v"as v)":"white_space(("Bool"|"Int")as d)white_space"|"white_space(bool_str as t)white_space"}*)"  
{ 
  let vk = init_ref ("cur_"^ Char.escaped v) (string_to_type d) t "=" t in 
  PRE (vk) 
}
| "(*-:{"("v"as v)":"white_space(("Bool"|"Int")as d)white_space"|"white_space("v" as l)white_space(bin_op_str as bop)white_space((ident|digits) as r)white_space"}*)"  
{ 
  let r' = try string_of_int (int_of_string r)
    with _ -> "pref"^r in
  let vk = init_ref ("cur_"^ Char.escaped v) (string_to_type d) ("cur_"^Char.escaped l) bop r' in 
  PRE (vk) 
}
| ":"  { COLON }
| ";"  { SEMI }
| "->" { ARROW }
| "<=" { LE }
| ">=" { GE }
| "&&" { AND }
| "||" { OR }
| "=" { EQ }
| "<>" { NE }
| "<" { LT }
| ">" { GT }
| '+' { PLUS }
| '-' { MINUS }
| '/' { DIV }
| types {TYPE}
| '*' { TIMES }
| '(' { LPAREN }
| ')' { RPAREN }
| ident as kw
    { try
      Hashtbl.find keyword_table kw
    with Not_found ->
      IDENT (kw)
    }
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