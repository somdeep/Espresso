type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq
type expr = (* Expressions *)
Literal of int (* 42 *)
| Id of string (* foo *)
| Binop of expr * op * expr (* a + b *)
| Assign of string * expr (* foo = 42 *)
| Call of string * expr list (* foo(1, 25 *)
| Noexpr (* for (;;) *)
type stmt = (* Statements *)
Block of stmt list (* { ... } *)
| Expr of expr (* foo = bar + 3; *)
| Return of expr (* return 42; *)
| If of expr * stmt * stmt (* if (foo == 42) {} else {} *)
| For of expr * expr * expr * stmt (* for (i=0;i<10;i=i+1) { ... } *)
| While of expr * stmt (* while (i<10) { i = i + 1 } *)
type func_decl = {
fname : string; (* Name of the function *)
formals : string list; (* Formal argument names *)
locals : string list; (* Locally defined variables *)
body : stmt list;
}
type program = string list * func_decl list (* global vars, funcs *)