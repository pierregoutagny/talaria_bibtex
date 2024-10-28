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
}

let ops = ['{' '}' '(' ')' ',' '=']
let space= [' ' '\t']
let nops = [^ '{' '}' '(' ')' ',' '=']
let bnops=nops # space # ['\n']
let key = bnops+

rule main = parse
| space { main lexbuf }
| '{'  { if is_surface() then (enter_curl(); LCURL) else read_text 0 (Buffer.create 10) lexbuf }
| '}'  { exit_curl(); RCURL }
| '('  { if is_surface() then (enter_paren(); LPAREN) else raise_syntax "Lexer - cannot use '(' inside entry" }
| ')'  { exit_paren(); RPAREN }
| '\n'  { Lexing.new_line lexbuf; main lexbuf}
| '@'(nops+ as s)  {KIND s}
| '='  { EQUAL }
| ',' { COMMA }
| key as s { KEY s }
| eof {EOF}
| _ as c { raise_unexpected_char "main" c }

and read_text n buf = parse
| '{' as c { Buffer.add_char buf c; read_text (n+1) buf lexbuf }
| '}' as c {
  if n=0
    then TEXT (Buffer.contents buf)
    else ( Buffer.add_char buf c; read_text (n-1) buf lexbuf )
  }
| [^ '{' '}']+ as s { Buffer.add_string buf s; read_text n buf lexbuf }
| eof { raise (SyntaxError ("Lexer - Unexpected EOF - unfinished field")) }
| _ as c { raise_unexpected_char "value" c }

{ assert (is_surface()) }
