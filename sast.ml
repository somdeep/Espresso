open Ast

type sexpr =
    SLiteral of int
  | SStrlit of string
  | SFloatlit of float
  | SBoolLit of bool
  | SCharlit of char
  | SId of string * typ
  | SBinop of sexpr * op * sexpr * typ
  | SUnop of uop * sexpr * typ
  | SAssign of sexpr * sexpr * typ
  | SCall of string * sexpr list * typ
  | SArrayAccess of sexpr * sexpr * typ
  | SHashmapAccess of string * sexpr * typ
  | SObjectAccess of sexpr * sexpr * typ
  | SNoexpr

(*type var_decl = Vdecl of typ * string*)

type sstmt =
    SBlock of sstmt list
  | SExpr of sexpr * typ
  | SReturn of sexpr * typ
  | SIf of sexpr * sstmt * sstmt
  | SFor of sexpr * sexpr * sexpr * sstmt
  | SWhile of sexpr * sstmt
  | SForeach of typ * string * string * sstmt
  | SBreak
  | SLocal of typ * string
  
type ftype = Reserved | Udf
type sfunc_decl = {
    styp : typ;
    sfname : string;
    sformals : formal list;
    sbody : sstmt list;
    sftype : ftype;
    scontext_class :  string;
    sthis_ptr : sexpr;
  }


type scbody = {
  sfields : var_decl list;
  smethods : sfunc_decl list;

}

type scdecl = {
  scname : string;
  scbody : scbody;
}

type sprogram =  {
	classes : scdecl list;
	functions : sfunc_decl list;
	main : sfunc_decl;
	reserved : sfunc_decl list;
}
