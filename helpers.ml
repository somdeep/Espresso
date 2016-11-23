(* helper functions defined here *)
open Ast


(* helper methods *)
let string_of_func_name s = 
    Funcname(s) -> s
    | _ -> ""