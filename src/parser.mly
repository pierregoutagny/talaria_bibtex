%token <string> TEXT
%token <string> KIND
%token STRING
%token <string> IDENT
%token <int> NUMBER
%token LCURL RCURL
%token LPAREN RPAREN
%token COMMA EQUAL CONCAT
%token EOF
%type < Fields.raw_entry_or_command list > main
%start main

%{ %}

%%

%public main:
  | l=list(entry_or_command) EOF { l }

entry_or_command:
  | e=entry          { Fields.RawEntry e }
  | s=string_command { Fields.RawString s }

(* Field values *)

field_value:
  | v=separated_nonempty_list(CONCAT, field_token) { v }

field_token:
  | n=NUMBER { Fields.FieldNum n }
  | s=TEXT   { Fields.FieldStr s }
  | v=IDENT  { Fields.FieldVar v }

(* String commands *)

string_command:
  | STRING LCURL var=IDENT EQUAL value=field_value RCURL { {Fields.var; value} }
  | STRING LPAREN var=IDENT EQUAL value=field_value RPAREN { {Fields.var; value} }

(* Entries *)

entry:
	| kind=KIND LCURL name=IDENT COMMA e=properties RCURL
	| kind=KIND LPAREN name=IDENT COMMA e=properties RPAREN
	{ {Fields.uid=name; kind; raw=e} }

properties:
	| key=IDENT EQUAL p=field_value COMMA e=properties
	  { Fields.Database.add (String.trim key) p e }
	| key=IDENT EQUAL p=field_value COMMA?
	  { Fields.Database.singleton (String.trim key) p }

