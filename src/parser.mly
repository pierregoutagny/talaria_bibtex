%token <string> TEXT
%token <string> KIND
%token STRING
%token <string> IDENT
%token <int> NUMBER
%token LCURL RCURL
%token LPAREN RPAREN
%token COMMA EQUAL CONCAT
%token EOF
%type < Fields.raw_entry Fields.Database.t> main
%start main

%{
  let add raw_entry database = Fields.Database.add raw_entry.Fields.uid raw_entry database
%}

%%

%public main:
	| entry=entry d=main { add entry d}
	| EOF {Fields.Database.empty}

entry:
	| kind=KIND LCURL name=IDENT COMMA e=properties RCURL
	| kind=KIND LPAREN name=IDENT COMMA e=properties RPAREN
	{ {Fields.uid=name; kind; raw=e} }

properties:
	| key=IDENT EQUAL p=field_value COMMA e=properties
	  { Fields.Database.add (String.trim key) p e }
	| key=IDENT EQUAL p=field_value COMMA?
	  { Fields.Database.singleton (String.trim key) p }

field_value:
  | v=separated_nonempty_list(CONCAT, field_token) { v }

field_token:
  | n=NUMBER { Fields.FieldNum n }
  | s=TEXT   { Fields.FieldStr s }
  | v=IDENT  { Fields.FieldVar v }
