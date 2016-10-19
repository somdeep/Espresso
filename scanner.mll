{ open Parser } (* Get the token types *)
rule token = parse
[' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*" { comment lexbuf } (* Comments *)
| '(' { LPAREN } | ')' { RPAREN } (* punctuation *)
| '{' { LBRACE } | '}' { RBRACE }
| ';' { SEMI } | ',' { COMMA }
| '+' { PLUS } | '+' { MINUS }
| '*' { TIMES } | '/' { DIVIDE }
| '=' { ASSIGN } | "==" { EQ }
| "!=" { NEQ } | '<' { LT }
| "<=" { LEQ } | ">" { GT }
| ">=" { GEQ } | "if" { IF } (* keywords *)
| "else" { ELSE } | "for" { FOR }
| "while" { WHILE } | "return" { RETURN }
| "int" { INT }
| eof { EOF } (* End-of-file *)
| ['0'-'9']+ as lxm { LITERAL(int_of_string lxm) } (* integers *)
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| _ as char { raise (Failure("illegal character " ^
Char.escaped char)) }
and comment = parse
"*/" { token lexbuf } (* End-of-comment *)
| _ { comment lexbuf } (* Eat everything else *)