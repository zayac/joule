{
open Lexing
open Parser

exception Syntax_Error of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }
}

let int = '-'? ['0'-'9'] ['0'-'9']*

(*let digit = ['0'-'9']*)
(*let frac = '.' digit**)
(*let exp = ['e' 'E'] ['-' '+']? digit+*)
(*let float = digit* frac? exp?*)

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '+']*
let var = '$' id

let str = ('"' [' ' '!' '#'-'~']* '"')
        | ('\'' [' ' '!'-'&' '('-'~']* '\'')

rule read = parse
  | white    { read lexbuf }
  | newline  { next_line lexbuf; read lexbuf }
  | int      { INT (int_of_string (Lexing.lexeme lexbuf)) }
(*  | float    { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }*)
  | "nil"    { NIL }
  | "true"   { TRUE }
  | "false"  { FALSE }
  | "not"    { NOT }
  | "or"     { OR }
  | "and"    { AND }
  | "none"   { NONE }
  | '{'      { LBRACE }
  | '}'      { RBRACE }
  | "(:"     { LSMILE }
  | ":)"     { RSMILE }
  | '('      { LPAREN }
  | ')'      { RPAREN }
  | '['      { LBRACKET }
  | ']'      { RBRACKET }
  | '<'      { LANGULAR }
  | '>'      { RANGULAR }
  | ':'      { COLON }
  (*| ';'      { SCOLON }*)
  | ','      { COMMA }
  | '|'      { BAR }
  (*| "<="     { LEQ }*)
  (*| '='      { EQ }*)
  | '~'      { NOMINAL }
  | str as s { STRING s }
  | id as i  { ID i }
  | var as i {
               let open Core.Std in
               VAR (String.suffix i (String.length i - 1)) 
             }
  | eof      { EOF }
  | _
      {
        let open Errors in
        let open Location in
        raise (Syntax_Error (error ~loc:(Some (curr lexbuf))
        (Printf.sprintf "unexpected character: '%s'" (Lexing.lexeme lexbuf))))
      }
