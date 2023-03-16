open Base 
open Ast

type pytypes = Int | Float | String | Bool | Generic of string | None | Callable of pytypes list * pytypes | Any




type function_info = Function of { funcname : string ; arguments: string list ;  arguments_annot: string list option ; return_annot: string option ; classname: string option }


let module2stmts : modul -> stmt list = function 
| Module x -> x.body
| _ -> raise (Failure "Not the Program") 


let expr2value :


let rec collect_funcdefs : stmt list -> string option -> function_info list 
= fun stmts classname -> List.fold_left stmts ~init: [] ~f:(fun acc stmt -> match stmt with FunctionDef _ -> get_annot_funcdef stmt classname:: acc | ClassDef x -> collect_funcdefs x.body classname @ acc | _ -> acc)


and get_annot_funcdef : stmt -> string option -> function_info
= fun stmt classname ->
  match stmt with 
  | FunctionDef x -> (
    let Arguments args = x.args in
    let args = args.args in 
    let arguments = List.map args ~f:(fun (Arg arg) -> arg.arg) in 
    let arguments_annot = List.map args ~f: (fun (Arg arg) -> arg.annotation) in
    let arguments_annot = 
      if List.length (List.filter arguments_annot ~f:(Option.is_some)) = List.length args then None 
      else Some arguments_annot in (* remove Some of elements *) 
    Function { funcname= x.name ; arguments ; arguments_annot ; return_annot = x.returns ; classname })
  | _ -> raise (Failure "Not FuctionDef")




(* and is_annot : stmt -> bool = function | FunctionDef x -> 
  if Option.is_none x.returns then false
  else (
    let Arguments args = x.arguments in 
    let args = args.args in 
    let is_annot_in_arg _ (Arg arg) = Option.is_some arg.annotation in 
    let result = List.map args ~f:(is_annot_in_arg) |> List.filter ~f:(Bool.(=) true) in 
    if List.length result = 0 then false else true)
  | _ -> raise (Failure "Not FuctionDef") *)


and get_args_n_return : stmt -> function_info = function 
  | FunctionDef x -> 
    let Argumetns args = x.arguments in
    let args = args.args in 
    let arguments = List.map args ~f:(fun Arg arg -> arg.name) in 
    let arguments_annot = List.map args ~f: (fun Arg arg -> arg.annotation) in
    let arguments_annot = if List.length (List.filter arguments_annot ~f:(Bool.(=) true)) = List.length args then None else arguments_annot in 
    Function { funcname: x.name ; arguments ; arguments_annot ; return_annot = x.returns ; classname = None }
  | _ -> raise (Failure "Not FuctionDef")


(* and get_returns : stmt list -> expr option list 
  = fun stmts -> 
    match stmts with 
    | [] -> []
    | hd :: tl -> (
      match hd with 
      | Return _ -> hd :: get_returns tl 
      | While x -> get_returns x.body @ get_returns tl
      | For x -> get_returns x.body @ get_returns tl
      | AsyncFor x -> get_returns x.body @ get_returns tl
      | If x -> get_returns (x.body @ x.orelse) @ get_returns tl
      | With x -> get_returns x.body @ get_returns tl
      | AsyncWith x -> get_returns x.body @ get_returns tl
      | Try x -> get_returns (x.body @ x.orelse) @ get_returns tl
      | _ -> get_returns tl
      ) *)
and get_type_by_default 

and get_type_by_annot : 














