{
open Parser

exception SyntaxError of string

let raise_syntax s = raise (SyntaxError s)
let raise_unexpected_char (location: string) (c: char) =
  raise_syntax (Printf.sprintf "unexpected character in %s: %C" location c)

type state_t = Surface | EntryParen | EntryCurl

let state: state_t ref = ref Surface

let is_surface () = !state = Surface

let enter_curl () = state := EntryCurl
let exit_curl () =
  match !state with
  | EntryCurl -> state := Surface
  | Surface -> raise_syntax "Lexer - too many '}'"
  | EntryParen -> raise_syntax "Lexer - cannot use '}' to close entry opened with '('"

let enter_paren () = state := EntryParen
let exit_paren () =
  match !state with
  | EntryParen -> state := Surface
  | Surface -> raise_syntax "Lexer - extra ')'"
  | EntryCurl -> raise_syntax "Lexer - cannot use ')' to close entry opened with '{'"

type text_delimiter = Curl | Quote

}

let ops = ['{' '}' '(' ')' '"' ',' '=' '#' '%' '\'']
let space= [' ' '\t']
let nops = [^ '{' '}' '(' ')' '"' ',' '=' '#' '%' '\'']
let allowed_char = nops # space # ['\n'] (* FIXME also remove ascii 0-37? cf bibtex.web:884. Maybe this should be defined positively *)
let digit = ['0'-'9']
let ident = (allowed_char # digit) allowed_char*
let number = digit+

rule main = parse
| space { main lexbuf }
| '{'  { if is_surface() then (enter_curl(); LCURL) else read_text Curl 0 (Buffer.create 10) lexbuf }
| '}'  { exit_curl(); RCURL }
| '('  { if is_surface() then (enter_paren(); LPAREN) else raise_syntax "Lexer - cannot use '(' inside entry" }
| ')'  { exit_paren(); RPAREN }
| '"'  { if is_surface() then raise_syntax "Lexer - cannot use '\"' outside entry" else read_text Quote 0 (Buffer.create 10) lexbuf }
| '\n'  { Lexing.new_line lexbuf; main lexbuf}
| '@'(ident as s)  {KIND s}
| '='  { EQUAL }
| ',' { COMMA }
| number as s { NUMBER s }
| ident as s { IDENT s }
| eof {EOF}
| _ as c { raise_unexpected_char "main" c }

and read_text delim n buf = parse
| '{' as c { Buffer.add_char buf c; read_text delim (n+1) buf lexbuf }
| '}' as c {
  if n=0
    then if delim=Curl then TEXT (Buffer.contents buf) else raise_syntax "Lexer - fields must be brace-balanced" (* TODO relax this balancing constraint? *)
    else ( Buffer.add_char buf c; read_text delim (n-1) buf lexbuf )
  }
| ('\\' _) as s { Buffer.add_string buf s; read_text delim n buf lexbuf }
| '"' as c {
  match delim with
  | Curl -> Buffer.add_char buf c; read_text delim n buf lexbuf
  | Quote -> if n=0
    then TEXT (Buffer.contents buf)
    else raise_syntax "Lexer - fields must be brace-balanced" (* TODO relax this balancing constraint? *)
  }
| [^ '{' '}' '"' '\\']+ as s { Buffer.add_string buf s; read_text delim n buf lexbuf }
| eof { raise (SyntaxError ("Lexer - Unexpected EOF - unfinished field")) }
| _ as c { raise_unexpected_char "value" c }

{ assert (is_surface()) }
