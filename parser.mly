/* Ocamlyacc parser for MicroC */

%{
open Ast
%}

%token CLASS
%token SEMI LPAREN RPAREN LBRACE RBRACE LSQUARE RSQUARE COMMA COLON
%token PLUS MINUS TIMES DIVIDE ASSIGN NOT MODULUS POWER
%token EQ NEQ LT LEQ GT GEQ TRUE FALSE AND OR
%token RETURN IF ELSE FOR WHILE FOREACH INT BOOL VOID STRING FLOAT
%token <int> LITERAL
%token <string> ID
%token <string> STRLIT
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
%right NOT NEG

%start program
%type <Ast.program> program

%%

program:
  cdecl EOF { $1 }

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
    | cbody fdecl { {
       fields = $1.fields;
       methods = $2 :: $1.methods
     } }
   
  

fdecl:
   data_typ ID LPAREN formals_opt RPAREN LBRACE formals_opt stmt_list RBRACE
     { { data_typ = $1;
	 fname = $2;
	 formals = $4;
	 locals = List.rev $7;
	 body = List.rev $8 } }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

formal_list:
    data_typ ID                   { [($1,$2)] }
  | formal_list COMMA data_typ ID { ($3,$4) :: $1 }

data_typ:
    typ  { Datatype($1) }
  |   array_typ { $1 }
typ:
    INT { Int }
  | BOOL { Bool }
  | VOID { Void }
  | STRING { String }
  | FLOAT { Float }

array_typ:
    typ LSQUARE brackets RSQUARE {Array($1, $3)}

brackets:
             { 1 }
  | brackets LSQUARE RSQUARE {$1 + 1 }  

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

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

expr:
    LITERAL          { Literal($1) }
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
  | MINUS expr %prec NEG { Unop(Neg, $2) }
  | NOT expr         { Unop(Not, $2) }
  | ID ASSIGN expr   { Assign($1, $3) }
  | ID LPAREN actuals_opt RPAREN { Call($1, $3) }
  | LPAREN expr RPAREN { $2 }


actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }
