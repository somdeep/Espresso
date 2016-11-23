open Ast
(* open Sast *)
(*open Helper*)

module StringMap = Map.Make (String)

(* define all built-in functions supported by espresso *)
let get_reserved_funcs = 
    let reserved_struct name return_type formals = 
		{
			fname 			= name;
			typ 	        = return_type;
			formals 		= formals;
			body 			= [];
		(*	func_type		= Sast.Reserved; *)
		}
	in
    let reserved_functions = [
        reserved_struct "print" (Datatype(Void));
    ] in
    reserved_functions

let check pgm = function
    Program(cdecl) ->
        (* generate reserved functions and obtain their map *)
        let reserved_functions = get_reserved_funcs in
        (*let reserved_func_maps = List.fold_left (fun map func -> StringMap.add func.fname) StringMap.empty reserved_functions in*)
        pgm
       (* 
        (* get class_map for the given class *)
        let class_map = get_class_map cdecl in

        (* perform semantic checking of all fields and methods. Generate an SAST *)
        let sast = get_sast class_map reserved cdecl in
        sast
    *)