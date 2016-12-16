(* Ocamllex scanner for MicroC *)

{ open Parser 

let unescape s =
    	Scanf.sscanf ("\"" ^ s ^ "\"") "%S%!" (fun x -> x)
}
let digit =  ['0'-'9']
let charl = ''' ( _? ) '''
let escape = '\\' ['\\' ''' '"' 'n' 'r' 't']
let ascii = ([' '-'!' '#'-'[' ']'-'~'])
let string = '"' ( (ascii | escape)* as s) '"'

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"     { comment lexbuf }           (* Comments *)
| "//"	   { sincom lexbuf }		(* Single-Line comments *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| '['      { LSQUARE }			(* Square brackets for Arrays *)
| ']'      { RSQUARE }
| ';'      { SEMI }
| ':'      { COLON }
| ','      { COMMA }
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| '='      { ASSIGN }
| '%'	   { MODULUS }
| '.'     { DOT }
| "**"	   { POWER }
| "=="     { EQ }
| "!="     { NEQ }
| '<'      { LT }
| "<="     { LEQ }
| '>'      { GT }
| ">="     { GEQ }
| "&&"     { AND }
| "||"     { OR }
| "!"      { NOT }
| "if"     { IF }
| "else"   { ELSE }
| "for"    { FOR }
| "while"  { WHILE }
| "foreach" { FOREACH }			(* Foreach loop *)
| "return" { RETURN }
| "int"    { INT }
| "bool"   { BOOL }
| "void"   { VOID }
| "String" { STRING }
| "float"  { FLOAT }
| "char"   { CHAR }
| "true"   { TRUE }
| "false"  { FALSE }
| "break"  { BREAK }
| "hashmap"{HASHMAP}
| "class"  { CLASS }
| "this"   { THIS }
| "lambda"  { LAMBDA }
| ['0'-'9']+ as lxm { LITERAL(int_of_string lxm) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| string { STRLIT(unescape s) }
| charl as lxm { CHARLIT(String.get lxm 1) }
| ['0'-'9']+['.']['0'-'9']+ as lxm { FLOATLIT(float_of_string lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }


and sincom = parse
  "\n" { token lexbuf }
| _    { sincom lexbuf }


