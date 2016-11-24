open Ast
open Sast 
(*open Helper*)

module StringMap = Map.Make (String)

type classMap = {
	field_map               : Ast.var_decl StringMap.t;
	func_map               : Ast.func_decl StringMap.t;
	(*builtFuncMap 	: sfunc_decl StringMap.t;*)
	cdecl 			        : Ast.cdecl;
}

let get_fully_qualified_name class_name fdecl = 
    let func_name = fdecl.fname in
        match func_name with
            "main" -> "main"
            |   _ -> class_name ^ "." ^ func_name

(* define all built-in functions supported by espresso *)
let get_reserved_funcs = 
    let reserved_struct name return_type formals = 
		{
			sfname 			= name;
			styp 	        = return_type;
			sformals 		= formals;
			sbody 			= [];
			sftype		    = Sast.Reserved;
		}
	in
    let reserved_functions = [
        reserved_struct "print" (Datatype(Void)) ([]);
    ] in
    reserved_functions
    
let get_class_maps cdecls = 
    let setup_class_map m cdecl =
        (* get all fields belonging to the class cdecl. Raise error if duplicates are found *)
        let field_maps m = function Vdecl(typ, name) ->
            if (StringMap.mem name m)
                then raise (Failure(" duplicate field name :" ^ name))
            else
                (StringMap.add name (Vdecl(typ, name)) m)
        in

        (* get all methods belonging to the class cdecl. Raise error if duplicates are found *)
        let func_maps m fdecl = 
            let func_full_name = get_fully_qualified_name cdecl.cname fdecl
            in
            if (StringMap.mem func_full_name m)
                then raise (Failure ("duplicate function : " ^ func_full_name))
            else
                (* check against reserved func maps here *)
                (StringMap.add (func_full_name) fdecl m)
        in

        (* check for duplicate classes and add their fields, methods respectively *)
        (if (StringMap.mem cdecl.cname m)
            then raise(Failure ("Duplicate Class Name : " ^ cdecl.cname ))
        else
            StringMap.add cdecl.cname
            {
                field_map = List.fold_left field_maps StringMap.empty cdecl.cbody.fields;
                func_map = List.fold_left func_maps StringMap.empty cdecl.cbody.methods;
                cdecl = cdecl;
            } 
            m
        )   
    in List.fold_left setup_class_map StringMap.empty cdecls
(*in*)


(* FUNCTION FOR GENERATING SAST  *)
let get_sast class_maps reserved cdecls =
	
	let find_main = (fun f -> match f.fname with "main" -> true | _ -> false) in
	
	let get_main func_list = 
		let mains = (List.find_all find_main func_list) in
		if List.length mains < 1 then 
		raise (Failure "Main not Defined") 
		else if List.length mains > 1 then 
		raise (Failure "too many mains defined")
		else List.hd mains 
	in
	
	let handle_cdecl cdecl = 
		let class_map = StringMap.find cdecl.cname class_maps in
		let func_list = List.fold_left (fun l f -> f::l) [] cdecl.cbody.methods	in	
		let scdecl = cdecl in
		(scdecl, func_list)
	in

	let iter_cdecls t c = 
	let scdecl = handle_cdecl c in 
	(fst scdecl :: fst t, snd scdecl @ snd t)
	in
	
	let scdecl_list, func_list = List.fold_left iter_cdecls ([], []) cdecls in
	let main = get_main func_list in
	{
		classes = [];
		functions = [];
		main = List.hd reserved;(*PLEASE CHANGE THIS, PLACEHOLDER*)
		reserved = reserved;
	}



let check pgm = function
    Program(cdecls) ->
        (* generate reserved functions and obtain their map *)
        let reserved_functions = get_reserved_funcs in
        (*let reserved_func_maps = List.fold_left (fun map func -> StringMap.add func.fname) StringMap.empty reserved_functions in*)
       
        (* get class_map for the given class *)
        let class_maps = get_class_maps cdecls in
        
        (* perform semantic checking of all fields and methods. Generate an SAST *)
        let sast = get_sast class_maps reserved_functions cdecls in
        sast
        
