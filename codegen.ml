(* Code generation: translate takes a semantically checked AST and
produces LLVM IR

LLVM tutorial: Make sure to read the OCaml version of the tutorial

http://llvm.org/docs/tutorial/index.html

Detailed documentation on the OCaml LLVM library:

http://llvm.moe/
http://llvm.moe/ocaml/

*)

open Sast 

module L = Llvm
module A = Ast
module Sem = Semant
module Hash = Hashtbl

open Llvm.MemoryBuffer
open Llvm_bitreader
module StringMap = Map.Make(String)
let values:(string, L.llvalue) Hash.t = Hash.create 50
let params:(string, L.llvalue) Hash.t = Hash.create 50
let class_types:(string, L.lltype) Hash.t = Hash.create 10
let class_field_indexes:(string, int) Hash.t = Hash.create 50


let context = L.global_context ()
let the_module = L.create_module context "Espresso Codegen"
let builder = L.builder context

let i32_t = L.i32_type context;;
let i8_t = L.i8_type context;;
let f_t = L.double_type context;;
let i1_t = L.i1_type context;;
let str_t = L.pointer_type i8_t;;
let i64_t = L.i64_type context;;
let void_t = L.void_type context;;



(*Code generation for a string*)
let rec string_gen llbuilder s = 
  L.build_global_stringptr s "tmp" llbuilder

(*Recursively return pointer type for array based on size*)
let rec get_ptr_type dt = match dt with
    A.ArrayType(t,0) -> get_llvm_type (A.Datatype(t))
  | A.ArrayType(t,1)  -> L.pointer_type (get_llvm_type (A.Datatype(t)))
  | A.ArrayType(t,i)  -> L.pointer_type (get_llvm_type (A.ArrayType(t,i-1)))
  | _ -> raise (Failure ("Invalid Array Pointer Type"))

(*return corresponding llvm types for Ast datatype - get_type*)
and get_llvm_type (dt : A.typ) = match dt with
  A.Datatype(Int) -> i32_t
| A.Datatype(Float) -> f_t
| A.Datatype(Bool)  -> i1_t
| A.Datatype(Char)  ->  i8_t
| A.Datatype(Void)  ->  void_t
| A.Datatype(String)  -> str_t
| Datatype(ObjTyp(name))  ->  L.pointer_type(find_class name)
| ArrayType(prim,i) ->  get_ptr_type (ArrayType(prim,(i)))
| _ -> raise (Failure ("llvm type not yet supported"))


(*Find out if a class/struct in llvm name exists, during object declaration*)
and find_class name = 
  try Hash.find class_types name
  with | Not_found  ->  raise(Failure ("Invalid class name")) 

(*Code generation for any expression that is an id*)
let rec id_gen llbuilder id dt isderef checkparam =
  if isderef then
    try Hash.find params id
    with | Not_found ->
    try let _val = Hash.find values id in
    L.build_load _val id llbuilder
    with | Not_found -> raise (Failure ("Unknown variable " ^ id))
  else
    try Hash.find values id
    with | Not_found ->
    try  
        let _val = Hash.find params id in
        if checkparam then raise (Failure ("Cannot assign to a parameter"))
        else _val
    with | Not_found -> raise (Failure ("Unknown variable " ^ id)) 

(*and assign_gen llbuilder lhs rhs dt = 
  let rhs_type = Sem.get_type_from_sexpr rhs in

  (*code generation for the lhs expression*)
  let lhs, isObjacc = match lhs with
  | Sast.SId(id,dt) ->  id_gen llbuilder id dt false false,false
  | SArrayAccess(st,exp,dt) -> raise (Failure ("yet to add"))
  | _ ->  raise (Failure ("LHS of an assignment must be stand-alone"))
  in*)




(*Code generation for an expression*)
and sexpr_gen llbuilder = function
    SLiteral(i) ->  L.const_int i32_t i
  | SBoolLit(b) -> if b then L.const_int i1_t 1 else L.const_int i1_t 0
  | SFloatlit(f)  ->  L.const_float f_t f
  | SStrlit(s)  ->   string_gen llbuilder s
  | SCharlit(c) ->  L.const_int i8_t  (Char.code c)
  | SId(name,dt)  ->  id_gen llbuilder name dt true false
  (*| SAssign(exp1,exp2,dt) ->  assign_gen llbuilder exp1 exp2 dt*)
  | _ -> raise (Failure "Not supported in codegen yet")


(*Code generation for a return statement*)
and return_gen llbuilder exp typ =
  match exp with 
    SNoexpr -> L.build_ret_void llbuilder
  | _ -> L.build_ret (sexpr_gen llbuilder exp) llbuilder


(*Code generation for local declaration*)
and local_gen llbuilder dt st  =
  let t = match dt with
          A.Datatype(A.ObjTyp(name)) -> find_class name
        | _ -> get_llvm_type dt
  in

  let alloc = L.build_alloca t st llbuilder in
  Hash.add values st alloc;
  alloc

(*Codegen for stmt*)
and stmt_gen llbuilder = function  
  SBlock st ->  List.hd(List.map (stmt_gen llbuilder) st)
| SExpr(exp,dt) -> sexpr_gen llbuilder exp
| SReturn(exp,typ) -> return_gen llbuilder exp typ
| SLocal(dt,st) ->  local_gen llbuilder dt st
|  _ -> raise (Failure ("unknown statement"))
  


(*Class stubs and class gen created here*)
let class_stub_gen s =
  let class_type = L.named_struct_type context s.scname in
  Hash.add class_types s.scname class_type

let class_gen s =
  let class_type = Hash.find class_types s.scname in
  let type_list = List.map  (function A.Vdecl(d,_) -> get_llvm_type d) s.scbody.sfields in
  let name_list = List.map (function A.Vdecl(_,s) -> s) s.scbody.sfields in

  (*Addition of a key field to all structs/classes, assuming serialized*)
  let type_list = i32_t :: type_list in
  let name_list = ".key" :: name_list in

  let type_array = (Array.of_list type_list) in
  List.iteri (fun typ name -> 
    let n = s.scname ^ "." ^ name in
    Hash.add class_field_indexes n typ; 
  ) name_list;
  L.struct_set_body class_type type_array true


(*Code generation for the main function of program*)
let main_gen main = 
  Hash.clear values;
  let ftype = L.function_type i32_t [||] in
  let func = L.define_function "main" ftype the_module in 
  let llbuilder = L.builder_at_end context (L.entry_block func) in
  
  let _ = stmt_gen llbuilder (SBlock(main.sbody)) in
  L.build_ret (L.const_int i32_t 0) llbuilder

let translate sprogram =
  (*match sprogram with *)

  (*(raise (Failure("In codegen")))*)

  let _ = List.map (fun s -> class_stub_gen s) sprogram.classes in
  let _ = List.map(fun s -> class_gen s) sprogram.classes in
  let _ = main_gen sprogram.main in 

  the_module