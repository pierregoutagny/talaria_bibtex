%token <string> TEXT
%token <string> KIND
%token <string> KEY
%token LCURL RCURL
%token LPAREN RPAREN
%token COMMA EQUAL
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
	| kind=KIND LCURL name=KEY COMMA e=properties RCURL
	| kind=KIND LPAREN name=KEY COMMA e=properties RPAREN
	{ {Fields.uid=name; kind; raw=e} }

properties:
	| key=KEY EQUAL p=TEXT COMMA e=properties
	  { Fields.Database.add (String.trim key) p e }
	| key=KEY EQUAL p=TEXT opt_comma
	  { Fields.Database.singleton (String.trim key) p }

opt_comma:
	| 	{()}
	| COMMA {()}
