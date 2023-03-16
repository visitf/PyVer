open Base 
open Ast


type function_info = Function of { name : string ; argument: string list ; return : string ; arguemnt_annot: string list option ; return_annot: string option }


let module2stmts : modul -> stmt list = function 
| Module x -> 
| _ -> raise (Failure "") 








