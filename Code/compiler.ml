open Ast.Simplify_IR
open Ast.Simplify_Value
open Mips
module Env = Map.Make (String)

type cinfo =
  { code : Mips.instr list
  ; env : Mips.addr Env.t
  ; fpo : int
  ; gen_label_counter : int
  ; top_loop_start : string
  ; top_loop_end : string
  }

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
      (fun acc a -> acc ^ fmt_expr a env ^ " , ")
      (Printf.sprintf "Call of %s(" name)
      args
    ^ ")"
  | Var var -> "Variable " ^ var
  | Assign (name, expr) -> Printf.sprintf "Variable : %s = %s" name (fmt_expr expr env)
;;

let fmt_instr begin_or_end instr c_info =
  let tmp =
    match instr with
    | Expr expr -> fmt_expr expr c_info.env
    | Decl var ->
      Printf.sprintf
        "(Implicit) Stack Space Reserved for Var (%s) at -FP(%d)"
        var
        c_info.fpo
    | While (cond, _, do_mode) ->
      let com =
        "While "
        ^ string_of_int c_info.gen_label_counter
        ^ " condition : ("
        ^ fmt_expr cond c_info.env
        ^ ") is true"
      in
      if do_mode then "Do " ^ com else com
    | If (cond, _, _) ->
      "If "
      ^ string_of_int c_info.gen_label_counter
      ^ " condition : ("
      ^ fmt_expr cond c_info.env
      ^ ") is true"
    | Break -> "Break"
    | Continue -> "Continue"
    | NestedBlock _ -> "None"
    | None -> "None"
    | For (cond, incre, _) ->
      let com =
        "For "
        ^ string_of_int c_info.gen_label_counter
        ^ " condition : ("
        ^ fmt_expr cond c_info.env
        ^ ") "
        ^ "\n# Increment :("
        ^ fmt_expr incre c_info.env
        ^ ")"
      in
      com
    | Return expr -> "Return " ^ fmt_expr expr c_info.env
    | Label label -> Printf.sprintf "Set of Label (%s)" label
    | Goto label -> Printf.sprintf "Goto Label (%s)" label
  in
  if String.equal "None" tmp
  then "None"
  else if begin_or_end == 0
  then "Start of Instr : " ^ tmp
  else "End of Instr : " ^ tmp
;;

let compile_value = function
  | Int number -> [ Li (Reg V0, number) ]
  | Bool b -> if b then [ Li (Reg V0, 1) ] else [ Li (Reg V0, 0) ]
  | Data str -> [ La (Reg V0, Lbl str) ]
  | Void -> [ Li (Reg V0, 0) ]
;;

let rec compile_expr e env =
  match e with
  | Value valeur -> compile_value valeur
  | Call (name, args) ->
    if String.contains name '.'
    then
      List.flatten
        (List.map
           (fun a -> compile_expr a env @ [ Addi (SP, SP, -4); Sw (V0, Mem (SP, 0)) ])
           args)
      @ [ Jal name; Addi (SP, SP, 4 * List.length args) ]
    else (
      let name = "_" ^ name in
      List.flatten
        (List.map
           (fun a -> compile_expr a env @ [ Addi (SP, SP, -4); Sw (V0, Mem (SP, 0)) ])
           args)
      @ [ Addi (SP, SP, -4); Jal name; Addi (SP, SP, -(4 * List.length args)) ])
  | Var var -> [ Lw (Reg V0, Env.find var env) ]
  | Assign (var, expr) -> compile_expr expr env @ [ Sw (V0, Env.find var env) ]
;;

let rec compile_instr instr c_info =
  match instr with
  | Expr e -> { c_info with code = c_info.code @ compile_expr e c_info.env }
  | Decl var ->
    { c_info with
      env = Env.add var (Mem (FP, -c_info.fpo)) c_info.env
    ; fpo = c_info.fpo + 4
    }
  | For (cond, incre, block) ->
    let for_uniq = "for" ^ string_of_int c_info.gen_label_counter in
    let for_incre = "for_incre" ^ string_of_int c_info.gen_label_counter in
    let compiled_block_info =
      compile_instr
        block
        { c_info with
          gen_label_counter = c_info.gen_label_counter + 1
        ; code = []
        ; top_loop_start = for_incre
        ; top_loop_end = "end_" ^ for_uniq
        }
    in
    { c_info with
      gen_label_counter = compiled_block_info.gen_label_counter + 1
    ; top_loop_start = c_info.top_loop_start
    ; top_loop_end = c_info.top_loop_end
    ; fpo = c_info.fpo + (compiled_block_info.fpo - c_info.fpo)
    ; code =
        c_info.code
        @ [ Jump_Lbl for_uniq ]
        @ compile_expr cond c_info.env
        @ [ Beq (V0, ZERO, "end_" ^ for_uniq) ]
        @ compiled_block_info.code
        @ [ Jump_Lbl for_incre ]
        @ compile_expr incre c_info.env
        @ [ J for_uniq ]
        @ [ Jump_Lbl ("end_" ^ for_uniq) ]
    }
  | While (cond, block, do_mode) ->
    let while_uniq = "while" ^ string_of_int c_info.gen_label_counter in
    let compiled_block_info =
      compile_instr
        block
        { c_info with
          gen_label_counter = c_info.gen_label_counter + 1
        ; code = []
        ; top_loop_start = (if do_mode then "Test_" ^ while_uniq else while_uniq)
        ; top_loop_end = "end_" ^ while_uniq
        }
    in
    { c_info with
      gen_label_counter = compiled_block_info.gen_label_counter + 1
    ; top_loop_start = c_info.top_loop_start
    ; top_loop_end = c_info.top_loop_end
    ; fpo = c_info.fpo + (compiled_block_info.fpo - c_info.fpo)
    ; code =
        (if do_mode
        then
          c_info.code
          @ [ Jump_Lbl while_uniq ]
          @ compiled_block_info.code
          @ compile_expr cond c_info.env
          @ [ Jump_Lbl ("Test_" ^ while_uniq) ]
          @ [ Beq (V0, ZERO, "end_" ^ while_uniq)
            ; J while_uniq
            ; Jump_Lbl ("end_" ^ while_uniq)
            ]
        else
          c_info.code
          @ [ Jump_Lbl while_uniq ]
          @ compile_expr cond c_info.env
          @ [ Beq (V0, ZERO, "end_" ^ while_uniq) ]
          @ compiled_block_info.code
          @ [ J while_uniq; Jump_Lbl ("end_" ^ while_uniq) ])
    }
  | If (cond, block_true, block_false) ->
    let else_uniq = "else" ^ string_of_int c_info.gen_label_counter in
    let end_uniq = "end_if" ^ string_of_int c_info.gen_label_counter in
    let true_block_info =
      compile_instr
        block_true
        { c_info with code = []; gen_label_counter = c_info.gen_label_counter + 1 }
    in
    let false_block_info =
      compile_instr
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
    ; fpo =
        c_info.fpo
        + (false_block_info.fpo - c_info.fpo)
        + (true_block_info.fpo - c_info.fpo)
    }
  | Continue -> { c_info with code = c_info.code @ [ J c_info.top_loop_start ] }
  | Break -> { c_info with code = c_info.code @ [ J c_info.top_loop_end ] }
  | NestedBlock block ->
    let compiled_block_info =
      compile_block
        block
        { c_info with gen_label_counter = c_info.gen_label_counter; code = [] }
    in
    { c_info with
      code = c_info.code @ compiled_block_info.code
    ; fpo = c_info.fpo + (compiled_block_info.fpo - c_info.fpo)
    }
  | Return expr ->
    { c_info with
      code =
        c_info.code
        @ compile_expr expr c_info.env
        @ [ Move (SP, FP); Lw (Reg RA, Mem (SP, 4)); Lw (Reg FP, Mem (SP, 8)); Jr RA ]
    }
  | None -> c_info
  | Goto lbl -> { c_info with code = c_info.code @ [ J lbl ] }
  | Label lbl -> { c_info with code = c_info.code @ [ Jump_Lbl lbl ] }

and compile_block block c_info =
  let rec compile_block_aux block c_info =
    match block with
    | [] -> c_info
    | hd :: tail ->
      compile_block_aux
        tail
        (compile_instr
           hd
           { c_info with code = c_info.code @ [ Coms (fmt_instr 0 hd c_info) ] })
  in
  compile_block_aux block c_info
;;

let cleanup_asm text =
  let _, ls =
    List.fold_left
      (fun acc elem ->
        let is_kept, list = acc in
        match elem with
        | Jr _ -> false, if is_kept then list @ [ elem ] else list
        | J _ -> false, if is_kept then list @ [ elem ] else list
        | Jump_Lbl _ -> true, list @ [ elem ]
        | _ -> is_kept, if is_kept then list @ [ elem ] else list)
      (true, [])
      text
  in
  ls
;;

let compile_def func count =
  match func with
  | Func (name, args, block) ->
    let name = "_" ^ name in
    let dec_args, _ =
      List.fold_left
        (fun (acc, nb) elm -> Env.add elm (Mem (FP, 12 + (nb * 4))) acc, nb + 1)
        (Env.empty, 0)
        (List.rev args)
    in
    let compiled_block_info =
      compile_block
        block
        { code = []
        ; env = dec_args
        ; fpo = 0
        ; gen_label_counter = count
        ; top_loop_start = ""
        ; top_loop_end = ""
        }
    in
    ( (if String.equal name "_main" then [ Jump_Lbl "main" ] else [])
      @ [ Jump_Lbl name
        ; Addi (SP, SP, -8)
        ; Sw (RA, Mem (SP, 4))
        ; Sw (FP, Mem (SP, 8))
        ; Move (FP, SP)
        ; Addi (SP, SP, -compiled_block_info.fpo)
        ]
      @ compiled_block_info.code
      @ [ Move (SP, FP)
        ; Lw (Reg RA, Mem (SP, 4))
        ; Lw (Reg FP, Mem (SP, 8))
        ; Addi (SP, SP, 8)
        ; Jr RA
        ]
    , compiled_block_info.gen_label_counter )
;;

let rec compile_prog prog counter =
  match prog with
  | [] -> [], counter
  | hd :: tail ->
    let func, new_counter = compile_def hd counter in
    let rest, prog_counter = compile_prog tail new_counter in
    func @ rest, prog_counter
;;

let compile ir simplify =
  let code, _ = compile_prog ir 0 in
  { text = cleanup_asm (code @ Baselib.builtins)
  ; data = List.of_seq (Seq.map (fun (str, label) -> label, Asciiz str) simplify)
  }
;;
