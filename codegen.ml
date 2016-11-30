open Llvm
open Hashtbl
open Sast
open Semant
open Ast

Let module S = Sast
Let module L = Llvm

let context = L.global_context ()
let the_module = L.create_module context "Espresso Language"
let builder = L.builder context

let tbl_of_struct_t:(string, lltype) Hashtbl.t = Hashtbl.create 100
let struc_index_tbl:(string, int) Hashtbl.t = Hashtbl.create 100
let tabl_of_vars:(string, llvalue) Hashtbl.t = Hashtbl.create 100
let tbl_of_formals:(string, llvalue) Hashtbl.t = Hashtbl.create 100


let find_ll_t datatype = match datatype with
  Datatype(Int) -> i32_t
  | Datatype(Bool) -> i1_t
  | Datatype(Void) -> void_t
  | Datatype(Float) -> f_t 
  | Datatype(Char_t) -> i8_t
  | _ -> raise(Failure ("Not a datatype"))


let translate sast =  
  let classes = sast.classes in
  let functions = sast.functions in 
  let main = sast.main in

  let big_func () = 
    let printf_typ = L.var_arg_function_type i32_t [|pointer_type|] in 

    let _ = L.declare_function "print" printf_typ the_module in 
    ()

  in
  let _ = big_func in

  let make_struct_tbl class_dec = (*adds struct type to table*)
    let struct_t = L.named_struct_type context class_dec.scname in
    Hashtbl.add tbl_of_struct_t class_dec.scname struct_t
  in
  let _ = List.map make_struct_tbl classes in


  (*CLASS STRUCTS*)
  let make_class_struct struc = 
    let s_typ = Hashtbl.find tbl_of_struct_t struc.scname in
    let t_list = List.map (function Field(d, _) -> find_ll_t d) struc.scname in 
    let name_list = List.map (function Field(_, s) -> s) struc.sfields in
    let t_list = i32_t :: t_list
    let name_list = ".key" :: name_list in
    let t_array = (Array.of_list t_list) in 
    List.iteri (
      fun a b ->
      let n = struc.scname ^ "." ^ b in
      Hashtbl.add struc_index_tbl n a;)
    name_list;
    L.struct_set_body s_typ t_array true 
  in
  let _ = List.map make_class_struct classes in 


  let function_def sfunc_decl =
    let func_name = sfunc_decl.sfname in
    let  TODO 


    (*FUNCTIONS*)
  let create_function sfunc_decl = 

(*...*)
  let rec make_statement llbuilder = function
    SBlock s -> List.hd (List.map (make_statement llbuilder) s)
  | SExpr 
  | SReturn 
  | SIf 
  | SFor 
  | SWhile 
  | SForeach 
  | SBreak 
  | SLocal 

  in
  let _ = List.map create_function

    (*MAIN*)

  let create_main main =
    Hashtbl.clear tbl_of_formals;
    Hashtbl.clear tabl_of_vars;
    let i_t = L.function_type i32_t[||] in
    let mfunc = L.define_function "main" i_t the_module in
    let llbuilder = L.builder_at_end context (L.entry_block mfunc)

    let _ = make_statement llbuilder (SBlock (main.sbody))

    in L.build_ret (L.const_int i32_t 0) llbuilder

    in _ = create_main main in



  the_module;

