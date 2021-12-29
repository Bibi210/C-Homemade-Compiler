open Ast
open Ast.Base_Value
open Ast.Base_IR
open Lexing
module Env = Map.Make (String)

exception Error of string * Lexing.position

let current_func_name = ref "None"

let gotos = ref []

let jump_lbl_env = ref Env.empty

let type_links_env =
  Hashtbl.of_seq
    (List.to_seq
       [
         (Int_t, Primitive_t);
         (Bool_t, Cast Int_t);
         (Str_t, Primitive_t);
         (Void_t, Primitive_t);
         (Pointer_t Int_t, Primitive_t);
         (Pointer_t Str_t, Primitive_t);
         (Pointer_t Bool_t, Cast (Pointer_t Int_t));
         (Pointer_t Void_t, Primitive_t);
       ])

let emit_warning msg pos =
  Printf.eprintf "Warning on line %d col %d: %s.\n" pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol)
    msg

let rec fmt_type = function
  | Int_t -> "Int"
  | Bool_t -> "Bool"
  | Void_t -> "Void"
  | Str_t -> "Str"
  | Func_t (return_type, _) -> fmt_type return_type
  | Var_t (v_type, _) -> "Var of type :" ^ fmt_type v_type
  | Pointer_t t -> " Pointer of : " ^ fmt_type t
  | Custom_t t -> Printf.sprintf "(Derived Type %s)" (fmt_type t)

let outside_of_custom_type = function Custom_t old_type -> old_type | x -> x

let type_equal a b pos =
  let rec type_equal_aux base_a a_aux a_aux_pos other is_casted =
    let a_aux = outside_of_custom_type a_aux in
    let other = outside_of_custom_type other in
    match a_aux = other with
    | true ->
        if is_casted then
          emit_warning
            (Printf.sprintf "Cast from %s to %s" (fmt_type base_a)
               (fmt_type other))
            a_aux_pos
        else ();
        true
    | false -> (
        match Hashtbl.find type_links_env a_aux with
        | Primitive_t ->
            if a_aux = other then (
              if is_casted then
                emit_warning
                  (Printf.sprintf "Cast from %s to %s" (fmt_type base_a)
                     (fmt_type other))
                  a_aux_pos
              else ();
              true)
            else false
        | Cast t -> type_equal_aux base_a t a_aux_pos other true
        | Derived t -> type_equal_aux base_a t a_aux_pos other false)
  in
  match type_equal_aux a a pos b false with
  | true -> true
  | false -> type_equal_aux b b pos a false

type val_type = { value : value; base_type : prog_type }

type expr_type = {
  expr : Base_IR.expr;
  base_type : prog_type;
  env : prog_type Env.t;
}

let analyze_value = function
  | Int x -> { value = Int x; base_type = Int_t }
  | Bool x -> { value = Bool x; base_type = Bool_t }
  | Str x -> { value = Str x; base_type = Str_t }
  | Void -> { value = Void; base_type = Void_t }

let is_assigned expr env =
  match expr with
  | Var x -> (
      match Env.find x env with
      | Var_t (_, assigned) -> assigned
      | _ -> failwith "Should Never Happen")
  | _ -> true

let rec analyze_expr expr env =
  match expr with
  | Syntax.Value ast_val ->
      let val_t = analyze_value ast_val.value in
      { expr = Value val_t.value; base_type = val_t.base_type; env }
  | Syntax.Call called_func -> (
      match Env.find_opt called_func.name env with
      | None ->
          let might_nat = native_func ^ called_func.name in
          if Option.is_some (Env.find_opt might_nat env) then
            analyze_expr (Syntax.Call { called_func with name = might_nat }) env
          else
            raise
              (Error (called_func.name ^ " is not defined", called_func.pos))
      | Some (Func_t (return_type, args_types)) ->
          if List.length called_func.args != List.length args_types then
            raise
              (Error
                 ( called_func.name
                   ^ Printf.sprintf
                       " called with wrong arguements number Expected : ( %d ) \
                        Given : ( %d )"
                       (List.length args_types)
                       (List.length called_func.args),
                   called_func.pos ))
          else
            let _counter = ref 0 in
            let args =
              List.map2
                (fun a b ->
                  let analyse_result = analyze_expr a env in
                  if type_equal analyse_result.base_type b called_func.pos then
                    let _ = incr _counter in
                    analyse_result.expr
                  else
                    raise
                      (Error
                         ( Printf.sprintf
                             "Function %s arguement number %d has wrong type \
                              Expected : ( %s ) Given : ( %s )"
                             called_func.name !_counter (fmt_type b)
                             (fmt_type analyse_result.base_type),
                           called_func.pos )))
                called_func.args args_types
            in
            {
              expr = Call (called_func.name, args);
              base_type = return_type;
              env;
            }
      | _ ->
          raise (Error (called_func.name ^ " is not Function", called_func.pos))
      )
  | Syntax.Var var -> (
      match Env.find_opt var.name env with
      | None ->
          raise
            (Error
               ( Printf.sprintf "The Variable: ( %s ) is not defined" var.name,
                 var.pos ))
      | Some (Func_t _) ->
          raise
            (Error
               ( Printf.sprintf "%s is not a Variable its a Function" var.name,
                 var.pos ))
      | Some (Var_t (var_type, assigned)) ->
          if assigned then { expr = Var var.name; base_type = var_type; env }
          else (
            emit_warning
              (Printf.sprintf "The Variable: ( %s ) might be uninitialized "
                 var.name)
              var.pos;
            { expr = Var var.name; base_type = var_type; env })
      | Some x ->
          Printf.printf "%s\n" (fmt_type x);
          failwith "Not Supposed To Happen")
  | Syntax.Assign assign -> (
      let str_var =
        match assign.var_name with Syntax.Lvar x -> x | Syntax.Lderef x -> x
      in
      let no_type_var =
        match assign.var_name with
        | Syntax.Lvar x -> Lvar x
        | Syntax.Lderef x -> Lderef x
      in
      match Env.find_opt str_var env with
      | None ->
          raise
            (Error
               ( Printf.sprintf "The ( %s ) Variable dont exist" str_var,
                 assign.pos ))
      | Some (Var_t (v_type, _)) ->
          let expr_result = analyze_expr assign.expr env in
          let new_env =
            Env.add str_var
              (Var_t (v_type, is_assigned expr_result.expr env))
              expr_result.env
          in
          let analyze_var =
            match assign.var_name with
            | Lvar x ->
                analyze_expr (Syntax.Var { name = x; pos = assign.pos }) new_env
            | Lderef x ->
                analyze_expr
                  (Syntax.Deref { var_name = x; pos = assign.pos })
                  env
          in
          if type_equal expr_result.base_type analyze_var.base_type assign.pos
          then
            {
              expr = Assign (no_type_var, expr_result.expr);
              base_type = expr_result.base_type;
              env = new_env;
            }
          else
            raise
              (Error
                 ( Printf.sprintf
                     "Wrong type assign Variable ( %s ) Expected ( %s ) Given \
                      ( %s ) "
                     str_var
                     (fmt_type analyze_var.base_type)
                     (fmt_type expr_result.base_type),
                   assign.pos ))
      | _ -> failwith "Wrong type assign")
  | Syntax.Deref x -> (
      match Env.find_opt x.var_name env with
      | Some (Var_t (var_t, assigned)) -> (
          match var_t with
          | Pointer_t t ->
              if assigned then { expr = Deref x.var_name; base_type = t; env }
              else
                raise
                  (Error
                     (Printf.sprintf "Deref of never initialized Pointer", x.pos))
          | t ->
              Printf.printf "%s\n" (fmt_type t);
              raise (Error ("Can't Deref cause its not a pointer", x.pos)))
      | None ->
          raise
            (Error
               ( Printf.sprintf
                   "(%s) does not exist Dereference is not possible" x.var_name,
                 x.pos ))
      | _ ->
          raise
            (Error
               ( Printf.sprintf "Dereference of (%s) is not possible" x.var_name,
                 x.pos )))
  | Syntax.Addr var -> (
      match Env.find_opt var.var_name env with
      | Some (Pointer_t t) ->
          { expr = Addr var.var_name; base_type = Pointer_t t; env }
      | Some (Var_t (t, _)) ->
          { expr = Addr var.var_name; base_type = Pointer_t t; env }
      | _ ->
          raise
            (Error
               ( Printf.sprintf
                   "(%s) does not exist Taking A Reference is not possible"
                   var.var_name,
                 var.pos )))

let rec analyze_instr intr env is_loop =
  match intr with
  | Syntax.Expr x ->
      let expr_type = analyze_expr x env in
      (Expr expr_type.expr, expr_type.env)
  | Syntax.Return syntax_return -> (
      let returned = analyze_expr syntax_return.expr env in
      match Env.find !current_func_name env with
      | Func_t (ret_type, _) ->
          if type_equal ret_type returned.base_type syntax_return.pos then
            (Return returned.expr, env)
          else
            raise
              (Error
                 ( Printf.sprintf
                     "Function (%s) as a wrong return type Expected (%s) Given \
                      (%s) "
                     !current_func_name (fmt_type ret_type)
                     (fmt_type returned.base_type),
                   syntax_return.pos ))
      | _ ->
          raise
            (Error
               ("Return Outside of a function not possible", syntax_return.pos))
      )
  | Syntax.Decl var -> (
      ( Decl var.var_name,
        match Env.find_opt var.var_name env with
        | None -> Env.add var.var_name (Var_t (var.var_type, false)) env
        | Some _ ->
            raise
              (Error
                 ( Printf.sprintf "Variable ( %s ) is already defined"
                     var.var_name,
                   var.pos )) ))
  | Syntax.While syntax_while ->
      let expr_type = analyze_expr syntax_while.cond env in
      let analyzed_block, _ =
        analyze_instr syntax_while.block expr_type.env true
      in
      ( While (expr_type.expr, analyzed_block, syntax_while.do_mode),
        expr_type.env )
  | Syntax.For syntax_for ->
      let cond_type = analyze_expr syntax_for.cond env in
      let incre_type = analyze_expr syntax_for.incre cond_type.env in
      let analyzed_block, _ =
        analyze_instr syntax_for.block cond_type.env true
      in
      (For (cond_type.expr, incre_type.expr, analyzed_block), cond_type.env)
  | Syntax.If syntax_if ->
      let expr_type = analyze_expr syntax_if.cond env in
      let analyzed_tr, _ =
        analyze_instr syntax_if.block_true expr_type.env is_loop
      in
      let analyzed_fal, _ =
        analyze_instr syntax_if.block_false expr_type.env is_loop
      in
      (If (expr_type.expr, analyzed_tr, analyzed_fal), expr_type.env)
  | Syntax.Break pos ->
      if is_loop then (Break, env)
      else raise (Error ("This Break is outside of any loop", pos))
  | Syntax.Continue pos ->
      if is_loop then (Continue, env)
      else raise (Error ("This Continue is outside of any loop", pos))
  | Syntax.NestedBlock block ->
      (NestedBlock (analyze_block block env is_loop), env)
  | Syntax.Label syntax_lbl -> (
      match Env.find_opt syntax_lbl.lbl !jump_lbl_env with
      | None ->
          jump_lbl_env := Env.add syntax_lbl.lbl syntax_lbl.pos !jump_lbl_env;
          (Label (!current_func_name ^ "_" ^ syntax_lbl.lbl), env)
      | Some prev_pos ->
          raise
            (Error
               ( Printf.sprintf
                   "This Label (%s) is already set on line %d col %d "
                   syntax_lbl.lbl prev_pos.pos_lnum prev_pos.pos_cnum,
                 syntax_lbl.pos )))
  | Syntax.Goto syntax_goto ->
      gotos := [ (syntax_goto.lbl, syntax_goto.pos) ] @ !gotos;
      (Goto (!current_func_name ^ "_" ^ syntax_goto.lbl), env)

and analyze_block block env is_loop =
  match block with
  | [] -> []
  | hd :: tail ->
      let analyzed_instr, updated_env = analyze_instr hd env is_loop in
      analyzed_instr :: analyze_block tail updated_env is_loop

let verify_gotos gotolbl_pos setlabels =
  List.iter
    (fun elm ->
      let lbl, pos = elm in
      match Option.is_some (Env.find_opt lbl setlabels) with
      | true -> ()
      | _ -> raise (Error ("This Goto use an unset label", pos)))
    gotolbl_pos

let analyze_def def env =
  match def with
  | Syntax.Func syntax_func -> (
      match Env.find_opt syntax_func.func_name env with
      | Some (Func_t _) ->
          raise
            (Error
               ( Printf.sprintf "Func (%s) already defined" syntax_func.func_name,
                 syntax_func.pos ))
      | _ ->
          let func_env =
            Env.add syntax_func.func_name
              (Func_t
                 ( syntax_func.func_type,
                   List.fold_left
                     (fun acc (elem_type, _) -> acc @ [ elem_type ])
                     [] syntax_func.args ))
              env
          in
          current_func_name := syntax_func.func_name;
          gotos := [];
          jump_lbl_env := Env.empty;
          let func_var_env, args =
            List.fold_left_map
              (fun func_env (var_type, var) ->
                (Env.add var (Var_t (var_type, true)) func_env, var))
              func_env syntax_func.args
          in
          let analyzed_body =
            analyze_block syntax_func.block func_var_env false
          in
          verify_gotos !gotos !jump_lbl_env;
          (Some (Func (syntax_func.func_name, args, analyzed_body)), func_env))
  | Syntax.Typedef syntax_typedef -> (
      match Hashtbl.find_opt type_links_env syntax_typedef.new_type with
      | Some _ ->
          raise (Error ("This type is already defined", syntax_typedef.pos))
      | None ->
          Hashtbl.add type_links_env syntax_typedef.new_type
            (Derived syntax_typedef.old_type);
          Hashtbl.add type_links_env (Pointer_t syntax_typedef.new_type)
            (Derived (Pointer_t syntax_typedef.old_type));
          (None, env))

let rec analyze_prog prog env =
  match prog with
  | [] -> ([], env)
  | hd :: tail -> (
      let func, new_env = analyze_def hd env in
      let rest, prog_env = analyze_prog tail new_env in
      match func with
      | Some func -> (func :: rest, prog_env)
      | None -> (rest, prog_env))

let analyze parsed =
  let analysed, env = analyze_prog parsed Baselib._types_ in
  if Option.is_some (Env.find_opt "main" env) then analysed
  else
    raise
      (Error
         ( "Main does not exist in this file ",
           { pos_fname = ""; pos_lnum = 0; pos_bol = 0; pos_cnum = 0 } ))
