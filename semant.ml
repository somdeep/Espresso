open Ast
open Sast 
(*open Helper*)

module StringMap = Map.Make (String)

type classMap = {
	field_map               : Ast.var_decl StringMap.t;
	func_map               	: Ast.func_decl StringMap.t;
	reserved_func_map 		: sfunc_decl StringMap.t;
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

let update_env_name env env_name = 
{
	env_class_maps = env.env_class_maps;
	env_class_map = env.env_class_map;
	env_name       = env_name;
	env_locals     = env.env_locals;
	env_parameters = env.env_parameters;
	env_return_type = env.env_return_type;
	env_in_for     = env.env_in_for;
	env_in_while   = env.env_in_while;
	env_in_foreach = env.env_in_foreach;
	env_reserved   = env.env_reserved;
}

let update_call_stack env in_for in_while in_foreach = 
{
	env_class_maps = env.env_class_maps;
	env_class_map = env.env_class_map;
	env_name       = env.env_name;
	env_locals     = env.env_locals;
	env_parameters = env.env_parameters;
	env_return_type = env.env_return_type;
	env_in_for     = in_for;
	env_in_while   = in_while;
	env_in_foreach = in_foreach;
	env_reserved   = env.env_reserved;
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
    
let get_class_maps cdecls reserved_maps = 
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
				reserved_func_map = reserved_maps;
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
	| 	Assign(expr1, expr2) -> check_assignment env expr1 expr2, env
	|	Binop(expr1, op, expr2) -> check_binop env expr1 op expr2, env
	|	Unop(op, expr) -> check_unop env op expr, env
	|	ArrayAccess(id, expr) -> check_array_access env id expr, env
	|	HashmapAccess(id, expr) -> check_hashmap_access env id expr, env
	|	Call(func_name, expr_list) -> check_call env func_name expr_list, env
	| 	Noexpr	->	SNoexpr, env

	
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
	|   SHashmapAccess(_,_,t) -> t
    |   SNoexpr -> Ast.Datatype(Void)

(* get a list of sexprs from a list of exprs *)
and get_sexprl_from_exprl env el =
  let env_ref = ref(env) in
  let rec helper = function
	  head::tail ->
		let a_head, env = get_sexpr_from_expr !env_ref head in
		env_ref := env;
		a_head::(helper tail)
	| [] -> []
  in (helper el), !env_ref

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
        raise (Failure ("Expected type " ^ Ast.string_of_datatype (env.env_return_type) ^ " but got " ^ Ast.string_of_datatype (type_sexpr)))

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

(* semantically verify a while statement *)
and check_while env expr st = 
	let old_val = env.env_in_while in
	let env = update_call_stack env env.env_in_for true env.env_in_foreach in

	let sexpr,_ = get_sexpr_from_expr env expr in
	let type_sexpr = get_type_from_sexpr sexpr in
	let sstmt, _ = parse_stmt env st in 
	let swhile = 
		if(type_sexpr = Datatype(Bool) || type_sexpr = Datatype(Void))
			then SWhile(sexpr,sstmt)
			else raise	(Failure ("Invalid while condition statement"))
	in
	let env = update_call_stack env env.env_in_for old_val env.env_in_foreach in
	swhile,env


(*semantically verify a for statement*)
(*MUST CONTAIN CONDITIONAL AS BOOLEAN*)
and check_for env exp1 exp2 exp3 st =
	let old_val = env.env_in_for in 
	let env = update_call_stack env true env.env_in_while env.env_in_foreach in

	let sexpr1,_ = get_sexpr_from_expr env exp1 in
	let sexpr2,_ = get_sexpr_from_expr env exp2 in
	let sexpr3,_ = get_sexpr_from_expr env exp3 in
	let for_body,_ = parse_stmt env st in
	let conditional_type = get_type_from_sexpr sexpr2 in
	let st_for =
		if(conditional_type = Datatype(Bool)) 
			then SFor(sexpr1,sexpr2,sexpr3,for_body)
		else raise (Failure ("Invalid For statement conditional"))
	in

	let env = update_call_stack env old_val env.env_in_while env.env_in_foreach in
	st_for, env


(*semantically check a foreach statement*)
and check_foreach env dt id1 id2 st =
	let old_val = env.env_in_foreach in
	let env = update_call_stack env env.env_in_for env.env_in_while true in

	if StringMap.mem id1 env.env_locals
	 	then raise (Failure ("Duplicate local declaration"))
	else
		 
		let new_env = {
			env_class_maps = env.env_class_maps;
			env_class_map = env.env_class_map;
			env_name = env.env_name;
			env_locals = StringMap.add id1 dt env.env_locals;
			env_parameters = env.env_parameters;
			env_return_type = env.env_return_type;
			env_in_for = env.env_in_for;
			env_in_while = env.env_in_while;
			env_in_foreach = env.env_in_foreach;
			env_reserved = env.env_reserved;
		}
		in
		
		let foreach_body,_ = parse_stmt new_env st in
		let type_id = get_id_data_type new_env id2 in

	let st_foreach =	
			if (dt = Datatype(Int) || dt = Datatype(Char) || dt = Datatype(Bool) || dt = Datatype(Float) || dt = Datatype(String) || dt = Datatype(Void))
				then SForeach(dt,id1,id2,foreach_body)
			else raise(Failure ("Foreach only works on primitives currently"))
	in

	let st_foreach_2 = 
			match type_id,dt with 
				ArrayType(t,_),Datatype(d) -> 
					if(t = d)
						then (st_foreach)
					else 
						raise(Failure ("Mismatch in array and iterator type for foreach"))	
			| _ ->	raise(Failure ("Need array type to walkthrough in foreach"))
	in

	let env = update_call_stack env env.env_in_for env.env_in_while old_val in
	st_foreach_2,new_env	
	
	



(*semantically verify a break statement*)
and check_break env =
	if env.env_in_for || env.env_in_while || env.env_in_foreach then
		SBreak,env
	else
		raise (Failure ("Break cannot be called outside of a loop"))

(* check types in assignments *)
and check_assignment env expr1 expr2 = 
	let sexpr1, _ = get_sexpr_from_expr env expr1 in
	let sexpr, _ = get_sexpr_from_expr env expr2 in
	let type_id = get_type_from_sexpr sexpr1 in match sexpr1 with 
	(******* add hashmap type and object type here ******)
		SId(_,_) | SArrayAccess(_,_,_) | SHashmapAccess(_,_,_) ->
									(let type_sexpr = get_type_from_sexpr sexpr in match (type_id, type_sexpr) with
										Datatype(ObjTyp(t1)), Datatype(ObjTyp(t2)) -> 
																	if t1 = t2 
																		then SAssign(sexpr1, sexpr, type_id) 
																		else raise (Failure ("illegal assignment from " ^ (string_of_datatype type_sexpr) ^ " to " ^ (string_of_datatype type_id)))
									|	_,_ -> if type_id = type_sexpr
												then SAssign(sexpr1, sexpr, type_id)
												else raise(Failure ("illegal assignment from " ^ (string_of_datatype type_sexpr) ^ " to " ^ (string_of_datatype type_id) ))
									)
	|	_ -> raise(Failure("lvalue required for assignment "))
	

(* semantically validate arithemtic operations *)
and check_arithmetic_ops sexpr1 sexpr2 op type1 type2 = match (type1, type2) with
(* Assuming that the lhs and rhs must have the same type *)
		(Datatype(Int), Datatype(Int)) -> SBinop(sexpr1, op, sexpr2, Datatype(Int))
	|	(Datatype(Float), Datatype(Float)) -> SBinop(sexpr1, op, sexpr2, Datatype(Float))	
	|	(Datatype(Char), Datatype(Char)) -> SBinop(sexpr1, op, sexpr2, Datatype(Char))
	|	_,_ -> raise(Failure("types " ^ (string_of_datatype type1) ^ " and " ^ (string_of_datatype type2) ^ " are incompatible for arithmetic operations "))

and check_relational_ops sexpr1 sexpr2 op type1 type2 = match (type1, type2) with
(* Assuming that the lhs and rhs must have the same type *)
		(Datatype(Int), Datatype(Int)) -> SBinop(sexpr1, op, sexpr2, Datatype(Bool))
	|	(Datatype(Float), Datatype(Float)) -> SBinop(sexpr1, op, sexpr2, Datatype(Bool))	
	|	(Datatype(Char), Datatype(Char)) -> SBinop(sexpr1, op, sexpr2, Datatype(Bool))
	|	_,_ -> raise(Failure("types " ^ (string_of_datatype type1) ^ " and " ^ (string_of_datatype type2) ^ " are incompatible for comparison operations "))

(* Assuming that the types on either side are equal - no implicit type casting/ type promotions *)
and check_equality_ops sexpr1 sexpr2 op type1 type2 = 
	if type1 = type2 
		then SBinop(sexpr1, op, sexpr2, Datatype(Bool))
		else raise(Failure("types " ^ (string_of_datatype type1) ^ " and " ^ (string_of_datatype type2) ^ " are incompatible for equality operations "))

(* supports only boolean types *)
and check_logical_ops sexpr1 sexpr2 op type1 type2 = match (type1, type2) with
		(Datatype(Bool), Datatype(Bool)) -> SBinop(sexpr1, op, sexpr2, Datatype(Bool))
	|	_,_ -> raise(Failure("types " ^ (string_of_datatype type1) ^ " and " ^ (string_of_datatype type2) ^ " are incompatible for logical operations. Only boolean types are supported. "))

(* check binary Arithmetic, relational & logical operations on expressions *)
and check_binop env expr1 op expr2 =
	let sexpr1, _ = get_sexpr_from_expr env expr1 in 
	let sexpr2, _ = get_sexpr_from_expr env expr2 in 
	let type1 = get_type_from_sexpr sexpr1 in
	let type2 = get_type_from_sexpr sexpr2 in 
	match op with
		Add | Sub | Mult | Div | Mod | Pow -> check_arithmetic_ops sexpr1 sexpr2 op type1 type2
	|	Lt | Leq | Gt | Geq -> check_relational_ops sexpr1 sexpr2 op type1 type2
	|	Equal | Neq -> check_equality_ops sexpr1 sexpr2 op type1 type2
	|	And | Or -> check_logical_ops sexpr1 sexpr2 op type1 type2
	|	_ -> raise (Failure("unknown binary operator "))

and check_unop env op expr = 
	let get_numeric_sunop oper sexpr typ_exp = match oper with
			Sub -> SUnop(oper, sexpr, typ_exp)
		|	_ -> raise (Failure (" illegal unary operator for numeric type " ^ (string_of_datatype typ_exp)))
	in
	let get_bool_sunop oper sexpr typ_expr = match oper with
			Not -> SUnop(oper, sexpr, typ_expr)
		|	_ -> raise (Failure (" illegal unary operator for boolean type "))
	in
	let sexpr, _ = get_sexpr_from_expr env expr in
	let type_sexpr = get_type_from_sexpr sexpr in
	match type_sexpr with 
		Datatype(Int) | Datatype(Float) -> get_numeric_sunop op sexpr type_sexpr
	|	Datatype(Bool) -> get_bool_sunop op sexpr type_sexpr
	|	_ -> raise(Failure("unary oparator can only be applied to Int, Float or Bool types "))

(* semantic check for array element access. Supporting only 1D arrays now *)
and check_array_access env id expr = 
	let sexpr, _ = get_sexpr_from_expr env expr in
	let type_sexpr = get_type_from_sexpr sexpr in match type_sexpr with
		Datatype(Int) -> (* check if id was declared as an array type *)
						(let type_id = get_id_data_type env id in match type_id with 
							ArrayType(t, _) -> SArrayAccess(id, sexpr, Datatype(t))
						|	_ -> raise(Failure(" identifier " ^ id ^ " does not belong to an ArrayType "))
						)
	|	_ -> raise(Failure(" array index must be an integer "))

(* semantically check for hashmap element access. Supporint only primitive types for hashmaps *)						
and check_hashmap_access env id expr = 
	let sexpr, _ = get_sexpr_from_expr env expr in
	let type_sexpr = get_type_from_sexpr sexpr in 
	let type_id = get_id_data_type env id in match (type_id, type_sexpr) with
			(Hashmaptype(t1, t2), Datatype(prim)) -> (if t1 = prim 
										(* NOTE: we return type t2 and not t1 as t2 -> type of the value *)
										then SHashmapAccess(id, sexpr, Datatype(t2))
										else raise(Failure("expected key of type " ^ (string_of_datatype (Datatype(t1))) ^ " but got type " ^ (string_of_datatype type_sexpr)  ))
									)
		|	(_, Datatype(prim))  -> raise(Failure("identifier " ^ id ^ " is not a valid hashmap type "))
		| 	(_, _) -> raise(Failure(" Hashmap currently supports only primitive "))	

(* check function call semantics *)
(* pass invoking object's environment in env if this is invoked by an object *)
and check_call env func_name expr_list =
	(* get class in corresponding env *)
	let context_class_map = try StringMap.find env.env_name env.env_class_maps with
		|	Not_found -> raise (Failure ("class was not found in the context of this function call "))
	in
	let sexpr_list, _ = get_sexprl_from_exprl env expr_list in
	(* check a given formal and actual parameter *)
	let get_actual_param formal_param param = 
		let formal_type = match formal_param with Formal(t, _) -> t | _ -> Datatype(Void) in
		let param_type = get_type_from_sexpr param in
		if formal_type = param_type 
			then param
			else raise (Failure("Type mismatch. Expected " ^ string_of_datatype formal_type ^ " but got " ^ string_of_datatype param_type))
	in

	(* check lengths of formal and passed parameters and get actual parameters *)
	let get_actual_params formal_params params = 
		let formal_len = List.length formal_params in
		let param_len = List.length params in
			if formal_len = param_len
				then List.map2 get_actual_param formal_params params
				else raise(Failure(" formal and actual parameters have unequal lengths "))
	in

	let func_full_name = env.env_name ^ "." ^ func_name in
	(* look for the function in the list of reserved functions. If it is not found there
		look at the list of member functions of the context_class *)
	try let func_handle = StringMap.find func_full_name context_class_map.reserved_func_map in
		let actuals_list = get_actual_params func_handle.sformals sexpr_list in
		SCall(func_full_name, actuals_list, func_handle.styp) with
	|	Not_found ->
		(* search the list of member functions *)
		try let func_handle = StringMap.find func_full_name context_class_map.func_map in
			let actuals_list = get_actual_params func_handle.formals sexpr_list in
			SCall(func_full_name, actuals_list, func_handle.typ) with
		|	Not_found -> raise(Failure("function " ^ func_name ^ " was not found "))



(* Parse a single statement by matching with different forms that a statement
    can take, and generate appropriate SAST node *)
and parse_stmt env stmt = match stmt with 
		Ast.Block blk -> check_block env blk
	|	Ast.Expr expr -> check_expr	env expr
	|	Ast.Return expr -> check_return env expr
	| 	Ast.If(expr,st1,st2) -> check_if env expr st1 st2
	|	Ast.While(expr,st) -> check_while env expr st
	|  	Ast.For(exp1,exp2,exp3,st) -> check_for env exp1 exp2 exp3 st
	|	Ast.Foreach(dt,exp1,exp2,st) -> check_foreach env dt exp1 exp2 st 
	|	Ast.Break -> check_break env
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
		classes = scdecl_list;
		functions = sfunctions_list; (* Should we remove main from this ? *)
		main = main;
		reserved = reserved;
	}





let check pgm = match pgm with(*function*)
	Program(cdecls) ->

	
	(* generate reserved functions and obtain their map *)
        let reserved_functions = get_reserved_funcs in
        let reserved_maps = List.fold_left (fun m func -> StringMap.add func.sfname func m) StringMap.empty reserved_functions in
       
        (* get class_map for the given class *)
      	let class_maps = get_class_maps cdecls reserved_maps in
        
        (* perform semantic checking of all fields and methods. Generate an SAST *)
   	let sast = get_sast class_maps reserved_functions cdecls in

	
        sast

