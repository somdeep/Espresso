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


let is_loop = ref false

let (br_block) = ref (L.block_of_value (L.const_int i32_t 0))
let (cont_block) = ref (L.block_of_value (L.const_int i32_t 0))



(*Code generation for a string*)
let rec string_gen llbuilder s = 
  L.build_global_stringptr s "tmp" llbuilder

(*Recursively return pointer type for array based on size*)
let rec get_ptr_type dt = match dt with
    A.ArrayType(t,0) -> get_llvm_type (A.Datatype(t))
  | A.ArrayType(t,i)  -> L.pointer_type (get_llvm_type (A.Datatype(t)))
  (*| A.ArrayType(t,i)  -> L.pointer_type (get_ptr_type (A.ArrayType(t,i-1)))*)
  | _ -> raise (Failure ("Invalid Array Pointer Type"))

(*return corresponding llvm types for Ast datatype - get_type*)
and get_llvm_type (dt : A.typ) = match dt with
  A.Datatype(Int) -> i32_t
| A.Datatype(Float) -> f_t
| A.Datatype(Bool)  -> i1_t
| A.Datatype(Char)  ->  i8_t
| A.Datatype(Void)  ->  void_t
| A.Datatype(String)  -> str_t
| A.Datatype(ObjTyp(name))  ->  L.pointer_type(find_class name)
| A.ArrayType(prim,i) ->  get_ptr_type (A.ArrayType(prim,(i)))
| _ -> raise (Failure ("llvm type not yet supported"))


(*Find out if a class/struct in llvm name exists, during object declaration*)
and find_class name = 
  try Hash.find class_types name
  with | Not_found  ->  raise(Failure ("Invalid class name")) 

(*Code generation for any expression that is an id*)
let rec id_gen llbuilder id dt isderef checkparam =
  if isderef then
    try 
      Hash.find params id
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


(*Code generation for Object Access*)
and obj_access_gen llbuilder lhs rhs d isAssign =

  let check_lhs lhs = 
    match lhs with
      SId(s,d)  ->  id_gen llbuilder s d false false
    | SArrayAccess(_,_,_)  ->  raise (Failure ("yet to do : array as lhs of object invocation"))
    | _ -> raise (Failure ("LHS of object access must be object")) 

  in 
  
  let rec check_rhs isAssign par_exp par_type rhs= 
    let par_str = A.string_of_object par_type in 
    match rhs with 

      SId(field,d)  ->
        let search_t = (par_str ^ "." ^ field) in 
        let field_index = Hash.find class_field_indexes search_t in
        let _val = L.build_struct_gep par_exp field_index field llbuilder in 
        let _val = match d with 
          Datatype(ObjTyp(_)) ->
            if not isAssign then _val
            else L.build_load _val field llbuilder
          | _ ->  
            if not isAssign then _val
            else L.build_load _val field llbuilder
        in
        _val  
    | _ ->  raise(Failure ("yet to do : rhs types in object access codegen"))
  in

  let lhs_type = Sem.get_type_from_sexpr lhs in 
  (*yet to do : treating arrays as objects? for length*)

  let lhs = check_lhs lhs in 
  let rhs = check_rhs isAssign lhs lhs_type rhs in
  rhs

(*Code generation for assign*)
and assign_gen llbuilder lhs rhs dt = 
  let rhs_type = Sem.get_type_from_sexpr rhs in

  (*code generation for the lhs expression*)
  let lhs, isObjacc = match lhs with
  | Sast.SId(id,dt) ->  id_gen llbuilder id dt false false,false
  | SArrayAccess(st,exp,dt) -> array_access_gen llbuilder st exp dt true, false
  | SObjectAccess(se, sel, d) -> obj_access_gen llbuilder se sel d false,true
  | _ ->  raise (Failure ("LHS of an assignment must be stand-alone"))
  in

  let rhs = match rhs with
  | Sast.SId(id,dt) ->  id_gen llbuilder id dt true false
  | Sast.SObjectAccess(e1,e2,d) ->  obj_access_gen llbuilder e1 e2 d true
  | _ ->  sexpr_gen llbuilder rhs
  in

  let rhs = match dt with 
    A.Datatype(ObjTyp(_)) ->  
      if isObjacc then rhs
      else L.build_load rhs "tmp" llbuilder
  | _ ->  rhs
  in
  let rhs = match dt,rhs_type with
    A.Datatype(Char),A.Datatype(Int)  ->  L.build_uitofp rhs i8_t "tmp" llbuilder
  | A.Datatype(Int),A.Datatype(Char)  ->  L.build_uitofp rhs i32_t  "tmp" llbuilder
  | _   -> rhs
  in

  ignore(L.build_store rhs lhs llbuilder);
  rhs



(*Code generation for array access*)
and array_access_gen llbuilder st exp dt isAssign = 
  let index = sexpr_gen llbuilder exp in
  let index = match dt with 
    A.Datatype(Char)  -> index
  | _ -> L.build_add index (L.const_int i32_t 1) "tmp" llbuilder 
  in
  (*let  arr = id_gen llbuilder st dt true false in*)
  let arr = sexpr_gen llbuilder st in
  (*ignore(raise (Failure (L.string_of_llvalue index))); *)
  let _val = L.build_gep arr [| index |] "tmp" llbuilder in 
  if isAssign 
    then _val
    else L.build_load _val "tmp" llbuilder


(*Codegen for initialising an array*)
and array_init llbuilder arr arr_len init_val start_pos =
  let new_block label = 
    let f = L.block_parent (L.insertion_block llbuilder) in
    L.append_block (L.global_context ()) label f
  in

  let bbcurr = L.insertion_block llbuilder in
  let bbcond = new_block "array.cond" in
  let bbbody = new_block "array.init" in
  let bbdone = new_block "array.done" in
  ignore(L.build_br bbcond llbuilder);
  L.position_at_end bbcond llbuilder;

  (*manage counter for length of array*)
  let counter = L.build_phi [L.const_int i32_t start_pos, bbcurr] "counter" llbuilder in
  L.add_incoming ((L.build_add counter (L.const_int i32_t 1) "tmp"  llbuilder), bbbody) counter;
  let cmp = L.build_icmp L.Icmp.Slt counter arr_len "tmp" llbuilder in
  ignore(L.build_cond_br cmp bbbody bbdone llbuilder);
  L.position_at_end bbbody llbuilder;

  (*Assign array position to init_val*)
  let arr_ptr = L.build_gep arr [| counter |] "tmp" llbuilder in
  ignore  (L.build_store init_val arr_ptr llbuilder);
  ignore  (L.build_br bbcond llbuilder);
  L.position_at_end bbdone llbuilder



(*Code generation for array creation, allocating space for the array*)
and array_create_gen llbuilder t exp_t el = 
  match exp_t with 
    A.ArrayType(A.Char,_) ->
      let e = el in 
      let size = (sexpr_gen llbuilder (SLiteral(e))) in
      let t = get_llvm_type t in
      let arr = L.build_array_malloc t size "tmp" llbuilder in
      let arr = L.build_pointercast arr (L.pointer_type t) "tmp" llbuilder in
      arr
  | _ ->
    let e = el in
    let t = get_llvm_type t in 
    let size = (sexpr_gen llbuilder (SLiteral(e))) in
    let size_t = L.build_intcast (L.size_of t) i32_t "tmp" llbuilder in
    let size = L.build_mul size_t size "tmp" llbuilder in
    let size_real = L.build_add size (L.const_int i32_t 1) "arr_size" llbuilder in 

    let  arr = L.build_array_malloc t size_real "tmp" llbuilder in 
    let  arr = L.build_pointercast arr (L.pointer_type t) "tmp" llbuilder in

    (*let arr_len_ptr = L.build_pointercast arr (L.pointer_type i32_t) "tmp" llbuilder in

    (*Store the length of the array*)
    ignore(L.build_store size_real arr_len_ptr llbuilder);
    array_init llbuilder arr_len_ptr  size_real (L.const_int i32_t 0) 0;*)
    arr



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
  | SArrayAccess(name,exp,dt) ->  array_access_gen llbuilder name exp dt false
  |SObjectAccess(e1,e2,d) ->  obj_access_gen llbuilder e1 e2 d true
  | _ -> raise (Failure "Not supported in codegen yet")

and call_gen  llbuilder func_name expr_list dt = match func_name with
    "print_int" | "print_char"
    | "print_float" | "print_string"
    | "print_char_array" -> print_gen llbuilder expr_list
    | _ -> (let params = List.map (sexpr_gen llbuilder) expr_list in
            (* func_name is unique since it is prepended with class_name always *)
            match dt with 
                Datatype(Void) -> L.build_call (func_lookup func_name) (Array.of_list params) "" llbuilder
              | _ -> L.build_call (func_lookup func_name) (Array.of_list params) "tmp" llbuilder
            )
  (*| _ -> raise(Failure("function " ^ func_name ^ " did not match any known function!"))*)

and func_lookup fname = match (L.lookup_function fname the_module) with
    None -> raise (Failure("function " ^ fname ^ " was not found!"))
  | Some func -> func


and print_gen llbuilder expr_list = 
    (* currently we don't support boolean types *)
    (* generate llvm code for the expression list *)
    let params = List.map (fun expr -> sexpr_gen llbuilder expr) expr_list in
    let param_types = List.map (Semant.get_type_from_sexpr) expr_list in
    let get_format_string dt = match dt with
    	A.ArrayType(Char, _) 	-> "%s"
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
	  L.build_call (func_lookup "printf") (Array.of_list (s :: params)) "tmp" llbuilder

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

(*Code generation for a for statement*)
and for_gen llbuilder init_st cond_st inc_st body_st =
  let old_val = !is_loop in
  is_loop := true;

  let the_func = L.block_parent (L.insertion_block llbuilder) in

  (*emit initialization code first*)
  let _ = sexpr_gen llbuilder init_st in

  (*Basically create the associated blocks for llvm : loop, inc, cond, afterloop*)
  let loop_bb = L.append_block context "loop" the_func in
  let inc_bb = L.append_block context "inc" the_func in
  let cond_bb = L.append_block context "cond" the_func in
  let after_bb = L.append_block context "afterloop" the_func in

  let _ = if not old_val then
    cont_block := inc_bb;
    br_block := after_bb;
  in

  (*hit the condition statement with a jump*)
  ignore (L.build_br cond_bb llbuilder);

  L.position_at_end loop_bb llbuilder;

  (*emit the code generated for the body of statements for the current loop*)
  ignore (stmt_gen llbuilder body_st);

  let bb = L.insertion_block llbuilder in
  L.move_block_after bb inc_bb;
  L.move_block_after inc_bb cond_bb;
  L.move_block_after cond_bb after_bb;
  ignore (L.build_br inc_bb llbuilder);

  (*Start physical insertion at inc*)
  L.position_at_end inc_bb llbuilder;

  (*emit the block of inc generated code*)
  let _ = sexpr_gen llbuilder inc_st in
  ignore (L.build_br cond_bb llbuilder);
  
  L.position_at_end cond_bb llbuilder;

  let cond_val = sexpr_gen llbuilder cond_st in 
  ignore (L.build_cond_br cond_val loop_bb after_bb llbuilder);

  L.position_at_end after_bb llbuilder;

  is_loop := old_val;

  L.const_null f_t

(*Code generation for a return statement*)
and return_gen llbuilder exp typ =
  match exp with 
    SNoexpr -> L.build_ret_void llbuilder
  | _ -> L.build_ret (sexpr_gen llbuilder exp) llbuilder


(*Code generation for local declaration*)
and local_gen llbuilder dt st  =
  let t = match dt with
          A.Datatype(A.ObjTyp(name)) -> find_class name
        (*| A.ArrayType(A.(),i) ->  array_create_gen llbuilder prim i st*)
        | A.ArrayType(prim,len)  ->  ignore (array_create_gen llbuilder (A.Datatype(prim)) dt len); get_llvm_type dt
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
| SFor(exp1,exp2,exp3,st) ->  for_gen llbuilder exp1 exp2 exp3 st
| SLocal(dt,st) ->  local_gen llbuilder dt st
|  _ -> raise (Failure ("unknown statement"))
  

let init_params func formals = 
    let formals = Array.of_list (formals) in

    Array.iteri (fun i v ->
      let name = formals.(i) in
      let name = A.string_of_formal_name name in
      L.set_value_name name v;
      Hash.add params name v;
    ) (L.params func)

(* function prototypes are declared here in llvm. This is used later to generate Call instructions *)
let func_stub_gen sfunc_decl =
  let params_types = List.rev (List.fold_left (fun l-> (function A.Formal(ty, _) -> get_llvm_type ty :: l )) [] sfunc_decl.sformals) in
 									      
let func_type = L.function_type (get_llvm_type sfunc_decl.styp) (Array.of_list params_types) in
(* raise(Failure("reached here!")) *)
  L.define_function sfunc_decl.sfname func_type the_module

(* function body is generated in llvm *)
let func_body_gen sfunc_decl = 
  Hash.clear values;
  Hash.clear params;
  let func = func_lookup sfunc_decl.sfname in
  
  (* this generates the entry point *)
  let llbuilder = L.builder_at_end context (L.entry_block func) in
  let _ = init_params func sfunc_decl.sformals in
  
  let _ = stmt_gen llbuilder (SBlock(sfunc_decl.sbody)) in
  if sfunc_decl.styp = Datatype(Void) 
		then ignore(L.build_ret_void llbuilder);
	()


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
  Hash.clear params;
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

  (* generate llvm code for function prototypes *)
  let _ = List.map (fun f -> func_stub_gen f) sprogram.functions in
  (* generate llvm code for the function body *)
  let _ = List.map (fun f -> func_body_gen f) sprogram.functions in

  let _ = main_gen sprogram.main in 

  the_module
