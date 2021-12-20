open Ast
open Ast.Base_Value
open Ast.Base_IR
open Baselib
open Lexing

exception Error of string * Lexing.position

type val_type =
  { value : value
  ; base_type : prog_type
  }

type expr_type =
  { expr : Base_IR.expr
  ; base_type : prog_type
  ; env : prog_type Baselib.Env.t
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
  | Func_t (return_type, _) -> fmt_type return_type
  | Var_t (v_type, _) -> fmt_type v_type
;;

let emit_warning msg pos =
  Printf.eprintf
    "Warning on line %d col %d: %s.\n"
    pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol)
    msg
;;

let is_assigned expr env =
  match expr with
  | Var x ->
    (match Env.find x env with
    | Var_t (_, assigned) -> assigned
    | _ -> failwith "Should Never Happen")
  | _ -> true
;;

let rec analyze_expr expr env =
  match expr with
  | Syntax.Value ast_val ->
    let val_t = analyze_value ast_val.value in
    { expr = Value val_t.value; base_type = val_t.base_type; env }
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
              if analyse_result.base_type == b
              then (
                let _ = incr _counter in
                analyse_result.expr)
              else
                raise
                  (Error
                     ( Printf.sprintf
                         "Function %s arguement number %d has wrong type Expected : ( %s \
                          ) Given : ( %s )"
                         called_func.name
                         !_counter
                         (fmt_type b)
                         (fmt_type analyse_result.base_type)
                     , called_func.pos )))
            called_func.args
            args_types
        in
        { expr = Call (called_func.name, args); base_type = return_type; env })
    | _ -> raise (Error (called_func.name ^ " is not Function", called_func.pos)))
  | Syntax.Var var ->
    (match Env.find_opt var.name env with
    | None ->
      raise
        (Error (Printf.sprintf "The Variable: ( %s ) is not defined" var.name, var.pos))
    | Some (Func_t _) ->
      raise
        (Error (Printf.sprintf "%s is not a Variable its a Function" var.name, var.pos))
    | Some (Var_t (var_type, assigned)) ->
      if assigned
      then { expr = Var var.name; base_type = var_type; env }
      else (
        emit_warning
          (Printf.sprintf "The Variable: ( %s ) might be uninitialized " var.name)
          var.pos;
        { expr = Var var.name; base_type = var_type; env })
    | Some _ -> failwith "Not Supposed To Happen")
  | Syntax.Assign assign ->
    (match Env.find_opt assign.var_name env with
    | None ->
      raise
        (Error
           (Printf.sprintf "The ( %s ) Variable dont exist" assign.var_name, assign.pos))
    | Some x ->
      let expr_result = analyze_expr assign.expr env in
      let new_env =
        Env.add
          assign.var_name
          (Var_t (expr_result.base_type, is_assigned expr_result.expr env))
          expr_result.env
      in
      let analyze_var =
        analyze_expr (Syntax.Var { name = assign.var_name; pos = assign.pos }) new_env
      in
      if expr_result.base_type == analyze_var.base_type
      then
        { expr = Assign (assign.var_name, expr_result.expr)
        ; base_type = expr_result.base_type
        ; env = new_env
        }
      else
        raise
          (Error
             ( Printf.sprintf
                 "Wrong type assign Variable ( %s ) Expected ( %s ) Given ( %s ) "
                 assign.var_name
                 (fmt_type x)
                 (fmt_type expr_result.base_type)
             , assign.pos )))
;;

let rec analyze_instr intr env is_loop =
  match intr with
  | Syntax.Expr x ->
    let expr_type = analyze_expr x env in
    Expr expr_type.expr, expr_type.env
  | Syntax.Return syntax_return -> Return (analyze_expr syntax_return.expr env).expr, env
  | Syntax.Decl var ->
    ( Decl var.var_name
    , (match Env.find_opt var.var_name env with
      | None -> Env.add var.var_name (Var_t (var.var_type, false)) env
      | Some _ ->
        raise
          (Error
             (Printf.sprintf "Variable ( %s ) is already defined" var.var_name, var.pos)))
    )
  | Syntax.While syntax_while ->
    let expr_type = analyze_expr syntax_while.cond env in
    let analyzed_block, _ = analyze_instr syntax_while.block expr_type.env true in
    While (expr_type.expr, analyzed_block, syntax_while.do_mode), expr_type.env
  | Syntax.For syntax_for ->
    let cond_type = analyze_expr syntax_for.cond env in
    let incre_type = analyze_expr syntax_for.incre cond_type.env in
    let analyzed_block, _ = analyze_instr syntax_for.block cond_type.env true in
    For (cond_type.expr, incre_type.expr, analyzed_block), cond_type.env
  | Syntax.If syntax_if ->
    let expr_type = analyze_expr syntax_if.cond env in
    let analyzed_tr, _ = analyze_instr syntax_if.block_true expr_type.env is_loop in
    let analyzed_fal, _ = analyze_instr syntax_if.block_false expr_type.env is_loop in
    If (expr_type.expr, analyzed_tr, analyzed_fal), expr_type.env
  | Syntax.Break pos ->
    if is_loop
    then Break, env
    else raise (Error ("This Break is outside of any loop", pos))
  | Syntax.Continue pos ->
    if is_loop
    then Continue, env
    else raise (Error ("This Continue is outside of any loop", pos))
  | Syntax.NestedBlock block -> NestedBlock (analyze_block block env is_loop), env

and analyze_block block env is_loop =
  match block with
  | [] -> []
  | hd :: tail ->
    let analyzed_instr, updated_env = analyze_instr hd env is_loop in
    analyzed_instr :: analyze_block tail updated_env is_loop
;;

let analyze parsed = analyze_block parsed Baselib._types_ false
