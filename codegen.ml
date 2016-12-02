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
let values:(string, L.llvalue) Hashtbl.t = Hashtbl.create 50
let params:(string, L.llvalue) Hashtbl.t = Hashtbl.create 50


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


(*Code generation for an expression*)
let rec sexpr_gen llbuilder = function
    SLiteral(i) ->  L.const_int i32_t i
  | SFloatlit(f)  ->  L.const_float f_t f  
  | _ -> raise (Failure "Not supported in codegen yet")

(*Code generation for a return statement*)
and return_gen llbuilder exp typ =
  match exp with 
    SNoexpr -> L.build_ret_void llbuilder
  | _ -> L.build_ret (sexpr_gen llbuilder exp) llbuilder


(*Codegen for stmt*)
and stmt_gen llbuilder = function
  
  SBlock st ->  List.hd(List.map (stmt_gen llbuilder) st)
| SReturn(exp,typ) -> return_gen llbuilder exp typ 
|  _ -> raise (Failure ("unknown statement"))
  


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

  let _ = main_gen sprogram.main in

  the_module