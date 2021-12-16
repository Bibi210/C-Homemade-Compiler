open Ast.Simplify_IR
open Ast.Simplify_Value
open Mips
module Env = Map.Make (String)

type cinfo =
  { code : Mips.instr list
  ; env : Mips.addr Env.t
  ; fpo : int
  ; gen_label_counter : int
  }

let compile_value = function
  | Int number -> [ Li (Reg V0, number) ]
  | Bool b -> if b then [ Li (Reg V0, 1) ] else [ Li (Reg V0, 0) ]
  | Data str -> [ La (Reg V0, Lbl str) ]
  | Void -> [ Li (Reg V0, 0) ]
;;

let fmt_val = function
  | Int number -> string_of_int number
  | Bool b -> string_of_bool b
  | Data str -> "Str Label " ^ str
  | Void -> "Void"
;;

let rec fmt_expr e env =
  match e with
  | Value valeur -> fmt_val valeur
  | Call (name, args) ->
    List.fold_left
      (fun acc a -> acc ^ " " ^ fmt_expr a env)
      (Printf.sprintf "Call of %s(" name)
      args
    ^ " )"
  | Var var -> "Variable " ^ var
  | Assign (name, expr) -> Printf.sprintf "Variable : %s = %s" name (fmt_expr expr env)
;;

let rec fmt_instr begin_or_end instr c_info =
  let tmp =
    match instr with
    | Expr expr -> fmt_expr expr c_info.env
    | Decl var ->
      Printf.sprintf "Stack Space Reserved for Var (%s) at -FP(%d)" var c_info.fpo
    | While (cond, block) ->
      "While "
      ^ string_of_int c_info.gen_label_counter
      ^ " condition : ("
      ^ fmt_expr cond c_info.env
      ^ ") is true"
    | If (cond, _, _) ->
      "If "
      ^ string_of_int c_info.gen_label_counter
      ^ " condition : ("
      ^ fmt_expr cond c_info.env
      ^ ") is true"
  in
  if String.equal "None" tmp
  then "None"
  else if begin_or_end == 0
  then "Start of Instr : " ^ tmp
  else "End of Instr : " ^ tmp
;;

let rec compile_expr e env =
  match e with
  | Value valeur -> compile_value valeur
  | Call (name, args) ->
    List.flatten
      (List.map
         (fun a -> compile_expr a env @ [ Addi (SP, SP, -4); Sw (V0, Mem (SP, 0)) ])
         args)
    @ [ Jal name; Addi (SP, SP, 4 * List.length args) ]
  | Var var -> [ Lw (Reg V0, Env.find var env) ]
  | Assign (var, expr) -> compile_expr expr env @ [ Sw (V0, Env.find var env) ]
;;

let rec compile_instr instr c_info =
  match instr with
  | Expr e -> { c_info with code = c_info.code @ compile_expr e c_info.env }
  | Decl var ->
    { c_info with
      code = c_info.code @ [ Addi (SP, SP, -4) ]
    ; env = Env.add var (Mem (FP, -c_info.fpo)) c_info.env
    ; fpo = c_info.fpo + 4
    }
  | While (cond, block) ->
    (* TODO *)
    let while_uniq = "while" ^ string_of_int c_info.gen_label_counter in
    let compiled_block_info =
      compile_block
        block
        { c_info with gen_label_counter = c_info.gen_label_counter + 1; code = [] }
    in
    { c_info with
      code =
        c_info.code
        @ [ Jump_Lbl while_uniq ]
        @ compile_expr cond c_info.env
        @ [ Beq (V0, ZERO, "end_" ^ while_uniq) ]
        @ compiled_block_info.code
        @ [ J while_uniq ]
        @ [ Jump_Lbl ("end_" ^ while_uniq) ]
    ; gen_label_counter = compiled_block_info.gen_label_counter + 1
    }
  | If (cond, block_true, block_false) ->
    let else_uniq = "else" ^ string_of_int c_info.gen_label_counter in
    let end_uniq = "end_if" ^ string_of_int c_info.gen_label_counter in
    let true_block_info =
      compile_block
        block_true
        { c_info with code = []; gen_label_counter = c_info.gen_label_counter + 1 }
    in
    let false_block_info =
      compile_block
        block_false
        { c_info with
          gen_label_counter = true_block_info.gen_label_counter + 1
        ; code = []
        }
    in
    { c_info with
      code =
        c_info.code
        @ compile_expr cond c_info.env
        @ [ Beq (V0, ZERO, else_uniq) ]
        @ true_block_info.code
        @ [ J end_uniq ]
        @ [ Jump_Lbl else_uniq ]
        @ false_block_info.code
        @ [ Jump_Lbl end_uniq ]
    }

and compile_block block c_info =
  match block with
  | [] -> c_info
  | hd :: tail ->
    compile_block
      tail
      (compile_instr
         hd
         { c_info with code = c_info.code @ [ Coms (fmt_instr 0 hd c_info) ] })
;;

let demo_before =
  [ Jump_Lbl "main"
  ; Addi (SP, SP, -8)
  ; Sw (RA, Mem (SP, 4))
  ; Sw (FP, Mem (SP, 8))
  ; Move (FP, SP)
  ]
;;

let demo_after =
  [ Move (SP, FP); Lw (Reg RA, Mem (SP, 4)); Lw (Reg FP, Mem (SP, 8)); Jr RA ]
;;

let demo_cinfo = { code = []; env = Env.empty; fpo = 0; gen_label_counter = 0 }

let compile ir simplify =
  let c_info = compile_block ir demo_cinfo in
  { text = demo_before @ c_info.code @ demo_after @ Baselib.builtins
  ; data = List.of_seq (Seq.map (fun (str, label) -> label, Asciiz str) simplify)
  }
;;
