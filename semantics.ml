open Ast
open Ast.Base_Value
open Ast.Base_IR
open Baselib

exception Error of string * Lexing.position

type val_type =
  { value : value
  ; base_type : prog_type
  }

type expr_type =
  { expr : Base_IR.expr
  ; base_type : prog_type
  }

let analyze_value = function
  | Int x -> { value = Int x; base_type = Int_t }
  | Bool x -> { value = Bool x; base_type = Bool_t }
  | Str x -> { value = Str x; base_type = Str_t }
  | Void -> { value = Void; base_type = Void_t }
;;

let rec fmt_type = function
  | Int_t -> "Int"
  | Bool_t -> "Bool"
  | Void_t -> "Void"
  | Str_t -> "Str"
  | Func_t (return_type, args_types) -> fmt_type return_type
;;

(*
let is_func name key env =
  if String.equal key name
  then (
    match Baselib.Env.find_opt name env with
    | None -> false
    | Some prog_type ->
      (match prog_type with
      | Func_t _ -> true
      | _ -> false))
  else false
;;
*)

let rec analyze_expr expr env =
  match expr with
  | Syntax.Value ast_val ->
    let val_t = analyze_value ast_val.value in
    { expr = Value val_t.value; base_type = val_t.base_type }
  | Syntax.Call called_func ->
    (match Baselib.Env.find_opt called_func.name env with
    | None -> raise (Error (called_func.name ^ " is not defined", called_func.pos))
    | Some (Func_t (return_type, args_types)) ->
      if List.length called_func.args != List.length args_types
      then
        raise
          (Error
             ( called_func.name
               ^ Printf.sprintf
                   " called with wrong arguements number Expected : ( %d ) Given : ( %d )"
                   (List.length args_types)
                   (List.length called_func.args)
             , called_func.pos ))
      else (
        let _counter = ref 0 in
        let args =
          List.map2
            (fun a b ->
              let analyse_result = analyze_expr a env in
              if analyse_result.base_type = b
              then (
                let _ = incr _counter in
                analyse_result.expr)
              else
                raise
                  (Error
                     ( Printf.sprintf
                         "Function %s arguement number %d has wrong type Expected : ( %s ) \
                          Given : ( %s )"
                         called_func.name
                         !_counter
                         (fmt_type b)
                         (fmt_type analyse_result.base_type)
                     , called_func.pos )))
            called_func.args
            args_types
        in
        { expr = Call (called_func.name, args); base_type = return_type })
    | _ -> raise (Error (called_func.name ^ " is not Function", called_func.pos)))
  | Syntax.Var var ->
    (match Env.find_opt var.name env with
    | None ->
      raise (Error (Printf.sprintf "The Variable: ( %s ) is not defined" var.name, var.pos))
    | Some (Func_t _) ->
      raise (Error (Printf.sprintf "%s is not a Variable its a Function" var.name, var.pos))
    | Some var_type -> { expr = Var var.name; base_type = var_type })
;;

let throw_type = function
  | x -> x.expr
;;

let analyze parsed = throw_type (analyze_expr parsed Baselib._types_)
