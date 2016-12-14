(* Abstract Syntax Tree and functions for printing it *)

type op = Add | Sub | Mult | Div | Eq | Neq | Lt | Leq | Gt | Geq | Mod | Pow |
          And | Or | Not

type uop = Sub | Not

(* These are the primitive datatypes supported by espresso, along with Object *)
type primitive = Int | Bool | Void | String | Float | Char | ObjTyp of string 
type typ = Datatype of primitive | Hashmaptype of primitive * primitive | ArrayType of primitive * int

(*type data_typ =  Datatype of typ | Any *)

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
  | Assign of expr * expr
  | Call of string * expr list
  | ArrayAccess of expr * expr
  | HashmapAccess of string * expr
  | ObjectAccess of expr * expr
  | Noexpr
  | This

type var_decl = Vdecl of typ * string

type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt
  | Foreach of typ * string * string * stmt
  | Break
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

type program = Program of cdecl list 

(* Pretty-printing functions *)

let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Pow -> "**"
  | Eq -> "=="
  | Neq -> "!="
  | Lt -> "<"
  | Leq -> "<="
  | Gt -> ">"
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"

let string_of_uop = function
    Sub -> "-"
  | Not -> "!"

let string_of_primitive = function
    Int -> "int" ^ " "
  | Bool -> "bool" ^ " "
  | Void -> "void" ^ " "
  | String -> "String" ^ " "
  | Float -> "float" ^ " "
  | Char -> "char" ^ " "
  | ObjTyp(s) -> "class " ^ s ^ " " 


(* Helper function to pretty print datatypes*)
let string_of_datatype = function
                ArrayType(p, sz)        -> (string_of_primitive p) ^ "[" ^ (string_of_int sz) ^ "]"
              | Datatype(p) -> string_of_primitive p
              | Hashmaptype(p1, p2) -> "hashmap <" ^ string_of_primitive p1 ^ "," ^ string_of_primitive p2 ^ ">"
    
        (*|     Any                     -> "Any" *)

(*let string_of_typ = function
  Datatype(p) -> string_of_primitive p
  | _ -> ""*)


let string_of_vdecl (var_decl) = match var_decl with
    Vdecl (t, id) -> string_of_datatype t ^ " " ^ id ^ ";\n"


let string_of_object = function
		Datatype(ObjTyp(s))	-> s
	| 	_ -> ""

(* Helper function to pretty print formal arguments *)
let string_of_formal = function
        Formal(t, name) -> (string_of_datatype t) ^ " " ^ name
    |   _               -> ""

let string_of_formal_name = function 
        Formal(t, name) -> name
    |   _ -> ""

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
  | Assign(e1, e2) -> string_of_expr e1 ^ " = " ^ string_of_expr e2
  | ArrayAccess(v, e) -> string_of_expr v ^ "[" ^ string_of_expr e ^ "]"
  | HashmapAccess(v, e) -> v ^ "<" ^ string_of_expr e ^ ">"
  | ObjectAccess(e1, e2) -> string_of_expr e1 ^ "." ^ string_of_expr e2
  | This -> "this"
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
  | Foreach(t,e1,e2,s) -> "foreach (" ^ string_of_datatype t ^ e1 ^
    " : " ^  e2 ^ ")\n" ^ string_of_stmt s 
  | Break -> "break;\n"
  | Local(t,s) -> string_of_datatype t ^  s ^ ";\n"



let string_of_func_decl func_decl =
  string_of_datatype func_decl.typ ^ " " ^
  func_decl.fname ^ "(" ^ String.concat ", " (List.map string_of_formal func_decl.formals) ^
  ")\n{\n" ^
 (* String.concat "" (List.map string_of_vdecl func_decl.locals) ^ *)
  String.concat "" (List.map string_of_stmt func_decl.body) ^
  "}\n"

let string_of_class class_decl = 
    "class " ^ class_decl.cname ^ " {\n" ^
    String.concat "" (List.map string_of_vdecl class_decl.cbody.fields) ^ "\n" ^ 
    String.concat "\n" (List.map string_of_func_decl class_decl.cbody.methods) ^
    "}\n"


let string_of_program program = match program with
  Program cdecls ->
    String.concat "" (List.map (string_of_class) cdecls)
