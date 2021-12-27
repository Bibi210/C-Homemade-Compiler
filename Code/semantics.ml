open Ast
open Ast.Base_Value
open Ast.Base_IR
open Lexing
module Env = Map.Make (String)

exception Error of string * Lexing.position

let current_func_name = ref "None"
let gotos = ref []
let jump_lbl_env = ref Env.empty

type val_type =
  { value : value
  ; base_type : prog_type
  }

type expr_type =
  { expr : Base_IR.expr
  ; base_type : prog_type
  ; env : prog_type Env.t
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
  | Var_t (v_type, _) -> "Var of type :" ^ fmt_type v_type
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
    (match Env.find_opt called_func.name env with
    | None ->
      let might_nat = native_func ^ called_func.name in
      if Option.is_some (Env.find_opt might_nat env)
      then analyze_expr (Syntax.Call { called_func with name = might_nat }) env
      else raise (Error (called_func.name ^ " is not defined", called_func.pos))
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
    | Some x ->
      Printf.printf "%s\n" (fmt_type x);
      failwith "Not Supposed To Happen")
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
  | Syntax.Label syntax_lbl ->
    (match Env.find_opt syntax_lbl.lbl !jump_lbl_env with
    | None ->
      jump_lbl_env := Env.add syntax_lbl.lbl syntax_lbl.pos !jump_lbl_env;
      Label (!current_func_name ^ "_" ^ syntax_lbl.lbl), env
    | Some prev_pos ->
      raise
        (Error
           ( Printf.sprintf
               "This Label (%s) is already set on line %d col %d "
               syntax_lbl.lbl
               prev_pos.pos_lnum
               prev_pos.pos_cnum
           , syntax_lbl.pos )))
  | Syntax.Goto syntax_goto ->
    gotos := [ syntax_goto.lbl, syntax_goto.pos ] @ !gotos;
    Goto (!current_func_name ^ "_" ^ syntax_goto.lbl), env

and analyze_block block env is_loop =
  match block with
  | [] -> []
  | hd :: tail ->
    let analyzed_instr, updated_env = analyze_instr hd env is_loop in
    analyzed_instr :: analyze_block tail updated_env is_loop
;;

let verify_gotos gotolbl_pos setlabels =
  List.iter
    (fun elm ->
      let lbl, pos = elm in
      match Option.is_some (Env.find_opt lbl setlabels) with
      | true -> ()
      | _ -> raise (Error ("This Goto use an unset label", pos)))
    gotolbl_pos
;;

let analyze_def def env =
  match def with
  | Syntax.Func syntax_func ->
    (match Env.find_opt syntax_func.func_name env with
    | Some (Func_t _) ->
      raise
        (Error
           ( Printf.sprintf "Func (%s) already defined" syntax_func.func_name
           , syntax_func.pos ))
    | _ ->
      let env =
        Env.add
          syntax_func.func_name
          (Func_t
             ( syntax_func.func_type
             , List.fold_left
                 (fun acc (elem_type, _) -> acc @ [ elem_type ])
                 []
                 syntax_func.args ))
          env
      in
      current_func_name := syntax_func.func_name;
      gotos := [];
      jump_lbl_env := Env.empty;
      let func_var_env, args =
        List.fold_left_map
          (fun env (var_type, var) -> Env.add var (Var_t (var_type, true)) env, var)
          env
          syntax_func.args
      in
      let analyzed_body = analyze_block syntax_func.block func_var_env false in
      verify_gotos !gotos !jump_lbl_env;
      Func (syntax_func.func_name, args, analyzed_body), env)
;;

let rec analyze_prog prog env =
  match prog with
  | [] -> [], env
  | hd :: tail ->
    let func, new_env = analyze_def hd env in
    let rest, prog_env = analyze_prog tail new_env in
    func :: rest, prog_env
;;

let analyze parsed =
  let analysed, _ = analyze_prog parsed Baselib._types_ in
  analysed
;;
