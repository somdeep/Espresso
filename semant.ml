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


type env = {
	env_class_maps: classMap StringMap.t;
	env_class_map : classMap;
	env_name      : string;
	env_locals    : typ StringMap.t;
	env_parameters: Ast.formal StringMap.t;
	env_return_type: typ;
	env_in_for    : bool;
	env_in_while  : bool;
    env_in_foreach: bool;
	env_reserved  : sfunc_decl list;
}


(* get complete function name prepended with the class *)
let get_fully_qualified_name class_name fdecl = 
    let func_name = fdecl.fname in
        match func_name with
            "main" -> "main"
            |   _ -> class_name ^ "." ^ func_name

let string_of_object = function
		Datatype(ObjTyp(s))	-> s
	| 	_ -> ""

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

let get_scdecl_from_cdecl sfdecls (cdecl) = 
	{
		scname = cdecl.cname;
        scbody = {sfields = cdecl.cbody.fields; smethods = sfdecls; }
	}



let rec get_sexpr_from_expr env expr = match expr with 
        Literal i -> SLiteral(i), env
    |   Strlit s -> SStrlit(s), env
    |   Floatlit f -> SFloatlit(f), env
    |   BoolLit b -> SBoolLit(b), env
    |   Charlit c -> SCharlit(c), env
	|	Id id -> SId(id, (get_id_data_type env id)), env
	| 	Assign(id, expr) -> check_assignment env id expr 

	
(* Update this function whenever SAST's sexpr is updated *)
and get_type_from_sexpr sexpr = match sexpr with 
        SLiteral(_) -> Ast.Datatype(Int)
    |   SStrlit(_) -> Ast.Datatype(String)
    |   SFloatlit(_) -> Ast.Datatype(Float)
    |   SBoolLit(_) -> Ast.Datatype(Bool)
    |   SCharlit(_) -> Ast.Datatype(Char)
    |   SId(_, t) -> t
    |   SBinop(_,_,_,t) -> t
    |   SUnop(_,_,t) -> t
    |   SAssign(_,_,t) -> t
    |   SCall(_,_,t) -> t
    |   SArrayAccess(_,_,t) -> t
    |   SNoexpr -> Ast.Datatype(Void)

(*semantically verify a block*)
and check_block env blk = match blk with 
		[] -> SBlock([SExpr(SNoexpr,Datatype(Void))]),env
	|	_  -> 
		let blk, _ = convert_stmt_list_to_sstmt_list env blk in
		SBlock(blk),env


(*semantically verify an Expression*)
and check_expr env expr =
	let sexpr,env = get_sexpr_from_expr env expr in
	let type_sexpr = get_type_from_sexpr sexpr in
	SExpr(sexpr, type_sexpr), env

(* semantically verify a return statement *)
and check_return env expr = 
    let sexpr, _ = get_sexpr_from_expr env expr in
    let type_sexpr = get_type_from_sexpr sexpr in
    if type_sexpr = env.env_return_type
        then SReturn(sexpr, type_sexpr), env
    else
        raise (Failure ("Expected type " ^ Ast.string_of_typ (env.env_return_type) ^ " but got " ^ Ast.string_of_typ (type_sexpr)))

(* semantically verify an if statement *)
and check_if env expr st1 st2 =
	let sexpr,_ = get_sexpr_from_expr env expr in
	let type_sexpr = get_type_from_sexpr sexpr in
	let if_body,_ =	parse_stmt env st1 in
	let else_body,_ = parse_stmt env st2 in
	if type_sexpr = Datatype(Bool)
		then SIf(sexpr,if_body,else_body), env
		else raise(Failure ("Invalid If expression type, must be Bool"))

 (* semantically verify local variable declaration *)
 and check_local env dt name =
 	if StringMap.mem name env.env_locals
	 	then raise (Failure ("Duplicate local declaration"))
	else
		let new_env = {
			env_class_maps = env.env_class_maps;
			env_class_map = env.env_class_map;
			env_name = env.env_name;
			env_locals = StringMap.add name dt env.env_locals;
			env_parameters = env.env_parameters;
			env_return_type = env.env_return_type;
			env_in_for = env.env_in_for;
			env_in_while = env.env_in_while;
			env_in_foreach = env.env_in_foreach;
			env_reserved = env.env_reserved;
		}in
		(match dt with
			Datatype(ObjTyp(s)) -> 
				(if not (StringMap.mem (string_of_object dt) env.env_class_maps)
					then raise((Failure ("Class type not defined")))
				 else
					SLocal(dt,name),new_env)
		|	_ -> SLocal(dt,name),new_env)

(* check types in assignments *)
and check_assignment env id expr = 
	let type_id = get_id_data_type env id in
	let sexpr,_ = get_sexpr_from_expr env expr in
	let type_sexpr = get_type_from_sexpr sexpr in
	match (type_id, type_sexpr) with
		Datatype(ObjTyp(t1)), Datatype(ObjTyp(t2)) -> if t1 = t2 
									then SAssign(id, sexpr, type_id) , env
									else raise (Failure ("illegal assignment from " ^ (string_of_datatype type_sexpr) ^ " to " ^ (string_of_datatype type_id)))
	|	_,_ -> if type_id = type_sexpr
					then SAssign(id, sexpr, type_id), env
					else raise(Failure ("illegal assignment from " ^ (string_of_datatype type_sexpr) ^ " to " ^ (string_of_datatype type_id) ))

(* Parse a single statement by matching with different forms that a statement
    can take, and generate appropriate SAST node *)
and parse_stmt env stmt = match stmt with 
		Ast.Block blk -> check_block env blk
	|	Ast.Expr expr -> check_expr	env expr
	|	Ast.Return expr -> check_return env expr
	| 	Ast.If(expr,st1,st2) -> check_if env expr st1 st2
	| 	Ast.Local(dt, name) -> check_local env dt name
		


(* Process the list of statements and return a list of sstmt nodes *)
and convert_stmt_list_to_sstmt_list env stmt_list = 
	let env_reference = ref(env) in
	let rec get_sstmt = function
	  hd::tl ->
		let sstmt, env = parse_stmt !env_reference hd in
		env_reference := env;
		sstmt::(get_sstmt tl)
	| [] -> []
	in 
	let sstmts = (get_sstmt stmt_list), !env_reference in
	sstmts


and get_id_data_type env id = 
	(* search local variables *)
	try StringMap.find id env.env_locals
	with | Not_found -> (* search function arguments *)
	try let param = StringMap.find id env.env_parameters in
		(function Formal(t, _) -> t) param
	with | Not_found -> (* search field members *)
	try let var_decl = StringMap.find id env.env_class_map.field_map in
		(function Vdecl(t, _) -> t) var_decl
	with | Not_found -> raise (Failure ("undefined identifier " ^ id))



(* Checks for the presence of a return statement when the signature indicates a 
	non-void return type *)
let is_return_present func_name func_body func_return_type =
    let leng = List.length func_body in	 
    if ((leng) = 0) then 
	(*if (func_return_type != Datatype(Void)) then (raise (Failure("No Statement"))) else (raise(Failure ("Blah blah")))*)
	match func_return_type with 
	  Datatype(Void) -> ()
	| _ ->	raise(Failure ("Empty function body where return was expected"))
    else    
    let last_stmt = List.hd (List.rev func_body) in match last_stmt, func_return_type with
            _,Datatype(Void) -> ()		
	|   SReturn(_,_), _ -> ()  (* There is a return statement *)
        |   _ -> raise(Failure "non-void function does not have a return statement\n")


(* Function that converts func_decl into sfunc_decl *)
let convert_fdecl_to_sfdecl class_maps reserved class_map cname fdecl = 
    let get_params_map m formal_node = match formal_node with 
			Formal(data_type, formal_name) -> (StringMap.add formal_name formal_node m) 
		| 	_ -> m
	in
	let env_params = List.fold_left get_params_map StringMap.empty fdecl.formals in
    let env = {
		env_class_maps 	= class_maps;
		env_class_map = class_map;
		env_name     	= cname;
		env_locals    	= StringMap.empty;
		env_parameters	= env_params;
		env_return_type	= fdecl.typ;
		env_in_for 		= false;
		env_in_while 	= false;
        env_in_foreach = false;
		env_reserved 	= reserved;
	} in
    (* the function's body is a list of statements. Semanticallu check each statement
       and generate the Sast node *)
    let fbody = fst (convert_stmt_list_to_sstmt_list env fdecl.body) in
    let fname = (get_fully_qualified_name cname fdecl) in
	ignore(is_return_present fname fbody fdecl.typ);
    {
		sfname 	    = (get_fully_qualified_name cname fdecl);
		styp 	    = fdecl.typ;
		sformals    = fdecl.formals;
		sbody 		= fbody;
		sftype		= Sast.Udf;
	}

	

(* FUNCTION FOR GENERATING SAST  *)
let get_sast class_maps reserved cdecls =
	
	(* look through SAST functions *)
	let find_main = (fun f -> match f.sfname with "main" -> true | _ -> false) in
	
	let get_main func_list = 
		let mains = (List.find_all find_main func_list) in
		if List.length mains < 1 then 
		raise (Failure ("Main not Defined")) 
		else if List.length mains > 1 then 
		raise (Failure ("too many mains defined"))
		else List.hd mains 
	in
	
	let handle_cdecl cdecl = 
		let class_map = StringMap.find cdecl.cname class_maps in
		 (* apply convert_fdecl_to_sfdecl on each method from the class and accumulate the corresponding sfdecls in the list *)
        let sfunc_list = List.fold_left (fun ls f -> (convert_fdecl_to_sfdecl class_maps reserved class_map cdecl.cname f) :: ls) [] cdecl.cbody.methods in
		let scdecl = get_scdecl_from_cdecl sfunc_list cdecl in
		(scdecl, sfunc_list)
	in

	let iter_cdecls t c = 
	let scdecl = handle_cdecl c in 
	(fst scdecl :: fst t, snd scdecl @ snd t)
	in
	
   
	let scdecl_list, sfunctions_list = List.fold_left iter_cdecls ([], []) cdecls in
	let main = get_main sfunctions_list in
	{
		classes = [];
		functions = sfunctions_list; (* Should we remove main from this ? *)
		main = List.hd reserved;(*PLEASE CHANGE THIS, PLACEHOLDER*)
		reserved = reserved;
	}





let check pgm = match pgm with(*function*)
	Program(cdecls) ->

	
	(* generate reserved functions and obtain their map *)
        let reserved_functions = get_reserved_funcs in
        (*let reserved_func_maps = List.fold_left (fun map func -> StringMap.add func.fname) StringMap.empty reserved_functions in*)
       
        (* get class_map for the given class *)
      	let class_maps = get_class_maps cdecls in
        
        (* perform semantic checking of all fields and methods. Generate an SAST *)
   	let sast = get_sast class_maps reserved_functions cdecls in

	
        sast

