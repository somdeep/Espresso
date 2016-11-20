(* Abstract Syntax Tree and functions for printing it *)

type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq | Mod | Pow |
          And | Or

type uop = Neg | Not

(* These are the primitive datatypes supported by espresso, along with Object *)
type typ = Int | Bool | Void | String | Float | ObjTyp of string | Char | Hashmaptype of typ * typ | ArrayType of typ * int | Any

(* type data_typ =  DataType of typ | Any *)

type formal = Formal of typ * string

type expr =
    Literal of int
  | Strlit of string
  | Floatlit of float
  | BoolLit of bool
  | Charlit of char
  | Id of string
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Assign of string * expr
  | Call of string * expr list
  | ArrayAccess of string * expr
  | Noexpr

type var_decl = Vdecl of typ * string

type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt
  | Foreach of typ * expr * expr * stmt
  | Break of expr
  | Local of typ * string
  
type func_decl = {
    typ : typ;
    fname : string;
    formals : formal list;
    body : stmt list;
  }


type cbody = {
  fields : var_decl list;
  methods : func_decl list;

}

type cdecl = {
  cname : string;
  cbody : cbody;
}

type program = Program of cdecl 

(* Pretty-printing functions *)

let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Pow -> "**"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"

let string_of_uop = function
    Neg -> "-"
  | Not -> "!"

let rec string_of_expr = function
    Literal(l) -> string_of_int l
  | Charlit(l) -> "'" ^ (String.make 1 l) ^ "'"
  | Strlit(s) -> s
  | Floatlit(s) -> string_of_float s
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | Id(s) -> s
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr -> ""

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
  | Break(e) -> "break;\n"

let string_of_typ = function
    Int -> "int"
  | Bool -> "bool"
  | Void -> "void"
  | String -> "String"
  | Float -> "float"
  | Char -> "char"


let string_of_vdecl (var_decl) = match var_decl with
    Vdecl (t, id) -> string_of_typ t ^ " " ^ id ^ ";\n"


(* Helper function to pretty pring datatypes*)
let string_of_datatype = function 
		ArrayType(p, sz)	-> (string_of_typ p) ^ "[" ^ (string_of_int sz) ^ "]"  
	|  	Any 			-> "Any"

(* Helper function to pretty print formal arguments *)
let string_of_formal = function
        Formal(t, name) -> (string_of_typ t) ^ " " ^ name
    |   _               -> ""

let string_of_func_decl func_decl =
  string_of_typ func_decl.typ ^ " " ^
  func_decl.fname ^ "(" ^ String.concat ", " (List.map string_of_formal func_decl.formals) ^
  ")\n{\n" ^
 (* String.concat "" (List.map string_of_vdecl func_decl.locals) ^ *)
  String.concat "" (List.map string_of_stmt func_decl.body) ^
  "}\n"

let string_of_program program = match program with
  Program class_decl ->
    String.concat "" (List.map string_of_vdecl class_decl.cbody.fields) ^ "\n" ^ 
    String.concat "\n" (List.map string_of_func_decl class_decl.cbody.methods)
