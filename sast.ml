open Ast

type sexpr =
    SLiteral of int
  | SStrlit of string
  | SFloatlit of float
  | SBoolLit of bool
  | SCharlit of char
  | SId of string
  | SBinop of sexpr * op * sexpr
  | SUnop of uop * sexpr
  | SAssign of string * sexpr
  | SCall of string * sexpr list
  | SArrayAccess of string * sexpr
  | SNoexpr

(*type var_decl = Vdecl of typ * string*)

type sstmt =
    SBlock of sstmt list
  | SExpr of sexpr
  | SReturn of sexpr
  | SIf of sexpr * sstmt * sstmt
  | SFor of sexpr * sexpr * sexpr * sstmt
  | SWhile of sexpr * sstmt
  | SForeach of typ * sexpr * sexpr * sstmt
  | SBreak of sexpr
  | SLocal of typ * string
  
type ftype = Reserved | Udf
type sfunc_decl = {
    typ : typ;
    fname : string;
    formals : formal list;
    body : sstmt list;
    ftype : ftype;
  }


type scbody = {
  fields : var_decl list;
  methods : sfunc_decl list;

}

type scdecl = {
  cname : string;
  cbody : scbody;
}

type sprogram =  {
	class : scdecl list;
	functions : sfunc_decl list;
	main : sfunc_decl;
	reserved : sfunc_decl list;
}