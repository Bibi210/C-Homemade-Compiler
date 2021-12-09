open Ast
module Simplify_Env = Map.Make (String)

let _counter = ref 0

type val_env =
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
;;

let simplify code =
  let a = simplify_expr code Simplify_Env.empty in
  a.expr, Simplify_Env.to_seq a.env
;;