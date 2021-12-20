open Ast
module Simplify_Env = Map.Make (String)

let _counter = ref 0
let _return_enconter = ref false
let _break_continue_enconter = ref false

type expr_env =
  { env : string Simplify_Env.t
  ; expr : Simplify_IR.expr
  }

let simplify_val value env =
  match value with
  | Base_Value.Int r -> { env; expr = Simplify_IR.Value (Int r) }
  | Base_Value.Bool r -> { env; expr = Simplify_IR.Value (Bool r) }
  | Base_Value.Str chaine ->
    (match Simplify_Env.find_opt chaine env with
    | Some x -> { env; expr = Simplify_IR.Value (Data x) }
    | None ->
      let n = "str_" ^ Int.to_string !_counter in
      incr _counter;
      let new_env = Simplify_Env.add chaine n env in
      { env = new_env; expr = Simplify_IR.Value (Data n) })
  | Void -> { env; expr = Simplify_IR.Value Void }
;;

let rec simplify_expr code env =
  match code with
  | Base_IR.Value t -> simplify_val t env
  | Base_IR.Call (name, args) ->
    let new_env, proccess_args =
      List.fold_left_map
        (fun acc elm ->
          let tmp = simplify_expr elm acc in
          tmp.env, tmp.expr)
        env
        args
    in
    { env = new_env; expr = Simplify_IR.Call (name, proccess_args) }
  | Base_IR.Var var -> { env; expr = Simplify_IR.Var var }
  | Base_IR.Assign (var, expr) ->
    let val_env = simplify_expr expr env in
    { expr = Simplify_IR.Assign (var, val_env.expr); env = val_env.env }
;;

let rec simplify_inst instr env =
  let prev_ret = !_return_enconter || !_break_continue_enconter in
  let tmp, env =
    match instr with
    | Base_IR.Expr x ->
      let val_env = simplify_expr x env in
      Simplify_IR.Expr val_env.expr, val_env.env
    | Base_IR.Decl x -> Simplify_IR.Decl x, env
    | Base_IR.While (cond, block, do_mode) ->
      let expr_env = simplify_expr cond env in
      let simplify_block, block_env = simplify_inst block expr_env.env in
      Simplify_IR.While (expr_env.expr, simplify_block, do_mode), block_env
    | Base_IR.For (cond, incre, block) ->
      let cond_env = simplify_expr cond env in
      let incre_env = simplify_expr incre cond_env.env in
      let simplify_block, block_env = simplify_inst block incre_env.env in
      Simplify_IR.For (cond_env.expr, incre_env.expr, simplify_block), block_env
    | Base_IR.If (cond, b_true, b_false) ->
      let expr_env = simplify_expr cond env in
      let simplify_block_true, true_block_env = simplify_inst b_true expr_env.env in
      let simplify_block_false, false_block_env = simplify_inst b_false true_block_env in
      ( Simplify_IR.If (expr_env.expr, simplify_block_true, simplify_block_false)
      , false_block_env )
    | Base_IR.Continue ->
      _break_continue_enconter := true;
      Simplify_IR.Continue, env
    | Base_IR.Break ->
      _break_continue_enconter := true;
      Simplify_IR.Break, env
    | Base_IR.NestedBlock b ->
      let simplify_block, new_env = simplify_block b env in
      Simplify_IR.NestedBlock simplify_block, new_env
    | Base_IR.Return expr ->
      _return_enconter := true;
      let expr_env = simplify_expr expr env in
      Simplify_IR.Return expr_env.expr, expr_env.env
    | Base_IR.None -> Simplify_IR.None, env
  in
  if prev_ret then Simplify_IR.None, env else tmp, env

and simplify_block block env =
  let rec simplify_block_aux block env =
    match block with
    | [] -> [], env
    | hd :: tail ->
      let instr, new_env = simplify_inst hd env in
      let code, output_env = simplify_block_aux tail new_env in
      instr :: code, output_env
  in
  let tmp = simplify_block_aux block env in
  _break_continue_enconter := false;
  tmp
;;

let simplify code =
  let code, labels = simplify_block code Simplify_Env.empty in
  code, Simplify_Env.to_seq labels
;;
