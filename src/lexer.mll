{
open Parser

exception SyntaxError of string

let surface = ref true

let enter_entry () = surface := false
let exit_entry () = surface := true

let raise_unexpected_char (location: string) (c: char) =
  raise (SyntaxError (Printf.sprintf "unexpected character in %s: %C" location c))

}

let ops = ['{' '}' ',' '=']
let space= [' ' '\t']
let nops = [^ '{' '}' ',' '=']
let bnops=nops # space # ['\n']
let key = bnops+

rule main = parse
| space { main lexbuf }
| '{'  { if !surface then (enter_entry(); LCURL) else read_value 0 (Buffer.create 10) lexbuf }
| '}'  { exit_entry(); RCURL }
| '\n'  { Lexing.new_line lexbuf; main lexbuf}
| '@'(nops+ as s)  {KIND s}
| '='  { EQUAL }
| ',' { COMMA }
| key as s { KEY s }
| eof {EOF}
| _ as c { raise_unexpected_char "main" c }

and read_value n buf = parse
| '{' as c { Buffer.add_char buf c; read_value (n+1) buf lexbuf }
| '}' as c {
  if n=0
    then TEXT (Buffer.contents buf)
    else ( Buffer.add_char buf c; read_value (n-1) buf lexbuf )
  }
| [^ '{' '}']+ as s { Buffer.add_string buf s; read_value n buf lexbuf }
| eof { raise (SyntaxError ("Lexer - Unexpected EOF - unfinished field")) }
| _ as c { raise_unexpected_char "value" c }

{ assert !surface }
