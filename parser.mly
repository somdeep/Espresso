/* Ocamlyacc parser for MicroC */

%{
open Ast
%}

%token CLASS
%token SEMI LPAREN RPAREN LBRACE RBRACE LSQUARE RSQUARE COMMA COLON
%token PLUS MINUS TIMES DIVIDE ASSIGN NOT MODULUS POWER
%token EQ NEQ LT LEQ GT GEQ TRUE FALSE AND OR
%token RETURN IF ELSE FOR WHILE FOREACH INT BOOL VOID STRING FLOAT CHAR BREAK HASHMAP
%token <int> LITERAL
%token <string> ID
%token <string> STRLIT
%token <char> CHARLIT
%token <float> FLOATLIT
%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE MODULUS POWER
%right NOT SUB

%start program
%type <Ast.program> program

%%

program:
  cdecls EOF { Program($1) }

cdecls:
cdecl_list       { List.rev $1 }

cdecl_list:
    cdecl       { [$1] }
|   cdecl_list cdecl    { $2::$1 }

cdecl:
   CLASS ID LBRACE cbody RBRACE
   { {
     cname = $2;
     cbody = $4
     } }

cbody:
      { {
       fields = [];
       methods = []
     } }
    | cbody vdecl { {
       fields = $2 :: $1.fields;
       methods = $1.methods
     } }
    | cbody func_decl { {
       fields = $1.fields;
       methods = $2 :: $1.methods
     } }
   

fname:
	ID { $1 }

func_decl:
   data_typ fname LPAREN formals_opt RPAREN LBRACE stmt_list RBRACE
     { { typ = $1;
	 fname = $2;
	 formals = $4;
	 body = List.rev $7 } }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

formal_list:
    formal                   { [$1] }
  | formal_list COMMA formal { $3 :: $1 }

formal:
 	data_typ ID {Formal($1,$2)}

data_typ:
    typ  { Datatype($1) }
  |   array_typ { $1 }
  |   hashmap_typ { $1 }
typ:
    INT { Int }
  | BOOL { Bool }
  | VOID { Void }
  | STRING { String }
  | FLOAT { Float }
  | CHAR { Char }
  | CLASS ID {ObjTyp($2)}

hashmap_typ:
  HASHMAP LT typ COMMA typ GT {Hashmaptype($3,$5)}

array_typ:
    typ LSQUARE LITERAL RSQUARE {ArrayType($1, $3)}

/* This is only for the class data members  */
vdecl:
   data_typ ID SEMI { Vdecl($1, $2) }

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    expr SEMI { Expr $1 }
  | RETURN SEMI { Return Noexpr }
  | RETURN expr SEMI { Return $2 }
  | LBRACE stmt_list RBRACE { Block(List.rev $2) }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
  | FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN stmt
     { For($3, $5, $7, $9) }
  | WHILE LPAREN expr RPAREN stmt { While($3, $5) }
  | FOREACH LPAREN data_typ expr COLON expr RPAREN stmt
     { Foreach($3, $4, $6, $8) }
  | BREAK SEMI { Break Noexpr }
  | data_typ ID  SEMI { Local($1,$2) }
 /* | data_typ ID ASSIGN expr SEMI { Local($1, $2, $4) } 
*/
expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

expr:
    LITERAL          { Literal($1) }
  | CHARLIT          { Charlit($1) }
  | STRLIT           { Strlit($1) }
  | FLOATLIT         { Floatlit($1) }
  | TRUE             { BoolLit(true) }
  | FALSE            { BoolLit(false) }
  | ID               { Id($1) }
  | expr PLUS   expr { Binop($1, Add,   $3) }
  | expr MINUS  expr { Binop($1, Sub,   $3) }
  | expr TIMES  expr { Binop($1, Mult,  $3) }
  | expr DIVIDE expr { Binop($1, Div,   $3) }
  | expr MODULUS expr { Binop($1, Mod,   $3) }
  | expr POWER expr { Binop($1, Pow,   $3) }
  | expr EQ     expr { Binop($1, Equal, $3) }
  | expr NEQ    expr { Binop($1, Neq,   $3) }
  | expr LT     expr { Binop($1, Less,  $3) }
  | expr LEQ    expr { Binop($1, Leq,   $3) }
  | expr GT     expr { Binop($1, Greater, $3) }
  | expr GEQ    expr { Binop($1, Geq,   $3) }
  | expr AND    expr { Binop($1, And,   $3) }
  | expr OR     expr { Binop($1, Or,    $3) }
  | MINUS expr %prec SUB { Unop(Sub, $2) }
  | NOT expr         { Unop(Not, $2) }
  | expr ASSIGN expr   { Assign($1, $3) }
  | ID LPAREN actuals_opt RPAREN { Call($1, $3) }
  | LPAREN expr RPAREN { $2 }
  | ID LSQUARE expr RSQUARE { ArrayAccess($1, $3) }

actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }
