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

and binop_gen llbuilder expr1 op expr2 dt = 
  let le1 = sexpr_gen llbuilder expr1 in
  let le2 = sexpr_gen llbuilder expr2 in

  let int_ops e1 op e2 = match op with
  		A.Add 		-> L.build_add e1 e2 "addtmp" llbuilder
	| 	A.Sub 		-> L.build_sub e1 e2 "subtmp" llbuilder
	| 	A.Mult 		-> L.build_mul e1 e2 "multmp" llbuilder
	| 	A.Div 		-> L.build_sdiv e1 e2 "divtmp" llbuilder
	| 	A.Mod 		-> L.build_srem e1 e2 "sremtmp" llbuilder
	| 	A.Eq 		  -> L.build_icmp L.Icmp.Eq e1 e2 "eqtmp" llbuilder
	| 	A.Neq 		-> L.build_icmp L.Icmp.Ne e1 e2 "neqtmp" llbuilder
	| 	A.Lt 		  -> L.build_icmp L.Icmp.Slt e1 e2 "lesstmp" llbuilder
	| 	A.Leq 		-> L.build_icmp L.Icmp.Sle e1 e2 "leqtmp" llbuilder
	| 	A.Gt		  -> L.build_icmp L.Icmp.Sgt e1 e2 "sgttmp" llbuilder
	| 	A.Geq 		-> L.build_icmp L.Icmp.Sge e1 e2 "sgetmp" llbuilder
	| 	A.And 		-> L.build_and e1 e2 "andtmp" llbuilder
	| 	A.Or 			-> L.build_or  e1 e2 "ortmp" llbuilder
	| 	_ 			-> raise (Failure("unsupported operator for integer arguments "))
	in 

  let float_ops e1 op e2 = match op with
		  A.Add 		-> L.build_fadd e1 e2 "flt_addtmp" llbuilder
	| 	A.Sub 		-> L.build_fsub e1 e2 "flt_subtmp" llbuilder
	| 	A.Mult 		-> L.build_fmul e1 e2 "flt_multmp" llbuilder
	| 	A.Div 		-> L.build_fdiv e1 e2 "flt_divtmp" llbuilder
	| 	A.Mod 		-> L.build_frem e1 e2 "flt_sremtmp" llbuilder
	| 	A.Eq 	    -> L.build_fcmp L.Fcmp.Oeq e1 e2 "flt_eqtmp" llbuilder
	| 	A.Neq 		-> L.build_fcmp L.Fcmp.One e1 e2 "flt_neqtmp" llbuilder
	| 	A.Lt 		  -> L.build_fcmp L.Fcmp.Ult e1 e2 "flt_lesstmp" llbuilder
	| 	A.Leq 		-> L.build_fcmp L.Fcmp.Ole e1 e2 "flt_leqtmp" llbuilder
	| 	A.Gt		  -> L.build_fcmp L.Fcmp.Ogt e1 e2 "flt_sgttmp" llbuilder
	| 	A.Geq 		-> L.build_fcmp L.Fcmp.Oge e1 e2 "flt_sgetmp" llbuilder
	| 	_ 			-> raise (Failure("unsupported operation for floating point arguments"))
	in 

  (* handle object comparisons *)
	let non_primitive_types e1 op e2 = match op with 
			A.Eq -> L.build_is_null e1 "tmp" llbuilder 
		| 	A.Neq -> L.build_is_not_null e1 "tmp" llbuilder 
		| 	_ 	-> raise (Failure("unsupported operator for objects "))
	in

  let match_types dt = match dt with
    A.Datatype(Float) -> float_ops le1 op le2 
  | A.Datatype(Int) | A.Datatype(Bool) | A.Datatype(Char) -> int_ops le1 op le2
  | A.Datatype(ObjTyp(_)) | A.ArrayType(_,_) | A.Hashmaptype(_,_) ->  non_primitive_types le1 op le2
  | _ -> raise(Failure("Unrecognized datatype! "))
  in
  match_types dt


and assign_gen llbuilder lhs rhs dt = 
  let rhs_type = Sem.get_type_from_sexpr rhs in

  (*code generation for the lhs expression*)
  let lhs, isObjacc = match lhs with
  | Sast.SId(id,dt) ->  id_gen llbuilder id dt false false,false
  | SArrayAccess(st,exp,dt) -> raise (Failure ("yet to add"))
  | _ ->  raise (Failure ("LHS of an assignment must be stand-alone"))
  in

  let rhs = match rhs with
  | Sast.SId(id,dt) ->  id_gen llbuilder id dt false false
  | Sast.SObjectAccess(_,_,_) ->  raise(Failure ("Yet to add object access to assign codegen"))
  | _ ->  sexpr_gen llbuilder rhs
  in

  let rhs = match dt with 
    A.Datatype(ObjTyp(_)) ->  raise (Failure ("yet to add to codegen"))
  | _ ->  rhs
  in
  let rhs = match dt,rhs_type with
    A.Datatype(Char),A.Datatype(Int)  ->  L.build_uitofp rhs i8_t "tmp" llbuilder
  | A.Datatype(Int),A.Datatype(Char)  ->  L.build_uitofp rhs i32_t  "tmp" llbuilder
  | _   -> rhs
  in

  ignore(L.build_store rhs lhs llbuilder);
  rhs

(*Code generation for an expression*)
and sexpr_gen llbuilder = function
    SLiteral(i) ->  L.const_int i32_t i
  | SBoolLit(b) -> if b then L.const_int i1_t 1 else L.const_int i1_t 0
  | SFloatlit(f)  ->  L.const_float f_t f
  | SStrlit(s)  ->   string_gen llbuilder s
  | SCharlit(c) ->  L.const_int i8_t  (Char.code c)
  | SId(name,dt)  ->  id_gen llbuilder name dt true false
  | SBinop(expr1, op, expr2, dt) -> binop_gen llbuilder expr1 op expr2 dt 
  | SAssign(exp1,exp2,dt) ->  assign_gen llbuilder exp1 exp2 dt
  | SCall(name, expr_list, dt) -> call_gen llbuilder name expr_list dt
  | _ -> raise (Failure "Not supported in codegen yet")

and call_gen  llbuilder func_name expr_list dt = match func_name with
    "print_int" | "print_char"
    | "print_float" | "print_string"
    | "print_char_array" -> print_gen llbuilder expr_list
  | _ -> raise(Failure("function " ^ func_name ^ " did not match any known function!"))

and get_lib_func fname = match (L.lookup_function fname the_module) with
    None -> raise (Failure("function " ^ fname ^ " was not found!"))
  | Some func -> func


and print_gen llbuilder expr_list = 
    (* currently we don't support boolean types *)
    (* generate llvm code for the expression list *)
    let params = List.map (fun expr -> sexpr_gen llbuilder expr) expr_list in
    let param_types = List.map (Semant.get_type_from_sexpr) expr_list in
    let get_format_string dt = match dt with
    	A.ArrayType(Char, 1) 	-> "%s"
		| 	A.Datatype(Int) 		-> "%d"
		| 	A.Datatype(Float) 	-> "%f"
		| 	A.Datatype(String) 	-> "%s"
		| 	A.Datatype(Char) 		-> "%c"
		| 	_ 						    -> raise (Failure("Datatype not supported by codegen!"))
    in
    let fmt_str = List.fold_left (fun s t -> s ^ get_format_string t) "" param_types in        
    let s = sexpr_gen llbuilder (SStrlit(fmt_str)) in
	  let zero = L.const_int i32_t 0 in 
	  let s = L.build_in_bounds_gep s [| zero |] "tmp" llbuilder in
	  L.build_call (get_lib_func "printf") (Array.of_list (s :: params)) "tmp" llbuilder

(*Code generation for if statement*)
and if_stmt_gen llbuilder exp then_st (else_st:Sast.sstmt) =
    let cond_val = sexpr_gen llbuilder exp in
    
    (*Write the first block, initial part, jump to relevant else parts as well*)
    let start_bb = L.insertion_block llbuilder in
    let the_func = L.block_parent start_bb in

    let then_bb = L.append_block context "then" the_func in

    (*Push out the 'then' output result/value*)
    L.position_at_end then_bb llbuilder;
    let _ = stmt_gen llbuilder then_st in


    (*codegen of then block modifies current block *)
    let new_then_bb = L.insertion_block llbuilder in 
    
    (*push out else block in new location of llvm block code*)
    let else_bb = L.append_block context "else" the_func in
    L.position_at_end else_bb llbuilder;
    let _ = stmt_gen llbuilder else_st in 


    let new_else_bb = L.insertion_block llbuilder in 
    
    let merge_bb = L.append_block context "ifcont" the_func in
    L.position_at_end merge_bb llbuilder;

    let else_bb_val = L.value_of_block new_else_bb in

    L.position_at_end start_bb llbuilder;
    ignore(L.build_cond_br cond_val then_bb else_bb llbuilder);

    L.position_at_end new_then_bb llbuilder;
    ignore(L.build_br merge_bb llbuilder);
    L.position_at_end new_else_bb llbuilder;
    ignore(L.build_br merge_bb llbuilder);

    L.position_at_end merge_bb llbuilder;

    else_bb_val 

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
| SIf(exp,st1,st2)  ->  if_stmt_gen llbuilder exp st1 st2
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

(* declare library functions *)
let construct_library_functions = 
  let printf_type = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
	let _ = L.declare_function "printf" printf_type the_module in
  ()

let translate sprogram =
  (*match sprogram with *)

  (*(raise (Failure("In codegen")))*)
  let _ = construct_library_functions in
  let _ = List.map (fun s -> class_stub_gen s) sprogram.classes in
  let _ = List.map(fun s -> class_gen s) sprogram.classes in
  let _ = main_gen sprogram.main in 

  the_module
