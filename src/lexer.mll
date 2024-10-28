{
open Parser

exception SyntaxError of string
}

let ops = ['{' '}' ',' '=']
let space= [' ' '\t']
let nops = [^ '{' '}' ',' '=']
let bnops=nops # space # ['\n']
let key = bnops+

rule main = parse
| space {main lexbuf}
| '{'  { read_fields lexbuf }
| '\n'  { Lexing.new_line lexbuf; main lexbuf}
| '@'(nops+ as s)  {KIND s}
| eof {EOF}
| _ as c { raise (SyntaxError (Printf.sprintf "unexpected character in main: %C" c)) }

and read_fields = parse
| space { read_fields lexbuf }
| '\n'  { Lexing.new_line lexbuf; read_fields lexbuf}
| '='  { EQUAL }
| ',' { COMMA }
| key as s { KEY s }
| '{' { read_value 0 (Buffer.create 10) lexbuf }
| eof { raise (SyntaxError ("Lexer - Unexpected EOF - unfinished entry")) }
| _ as c { raise (SyntaxError (Printf.sprintf "unexpected character in fields: %C" c)) }

and read_value n buf = parse
| '{' as c { Buffer.add_char buf c; read_value (n+1) buf lexbuf }
| '}' as c {
  if n=0
    then TEXT (Buffer.contents buf)
    else ( Buffer.add_char buf c; read_value (n-1) buf lexbuf )
  }
| [^ '{' '}']+ as s { Buffer.add_string buf s; read_value n buf lexbuf }
| eof { raise (SyntaxError ("Lexer - Unexpected EOF - unfinished field")) }
| _ as c { raise (SyntaxError (Printf.sprintf "unexpected character in value: %C" c)) }
