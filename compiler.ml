open Ast.Simplify_IR
open Ast.Simplify_Value
open Mips
module Env = Map.Make (String)

let rec compile_value = function
  | Int number -> [ Li (Reg V0, number) ]
  | Bool b -> if b then [ Li (Reg V0, 1) ] else [ Li (Reg V0, 0) ]
  | Data str -> [ La (Reg V0, Lbl str) ]
  | Void -> compile_value (Int 0)
;;

let rec compile_expr e env =
  match e with
  | Value valeur -> compile_value valeur
  | Call (name, args) ->
    [ Addi (SP, SP, -4); Sw (RA, Mem (SP, 0)) ]
    @ List.flatten
        (List.map
           (fun a -> compile_expr a env @ [ Addi (SP, SP, -4); Sw (V0, Mem (SP, 0)) ])
           args)
    @ [ Jal name; Addi (SP, SP, 4 * (List.length args + 1)); Lw (Reg RA, Mem (SP, -4)) ]
  | Var var -> [ Lw (Reg V0, Env.find var env) ]
;;

let demo = [ Jr (Reg RA) ]

let compile ir simplify =
  { text = compile_expr ir Env.empty @ demo @ Baselib.builtins
  ; data = List.of_seq (Seq.map (fun (str, label) -> label, Asciiz str) simplify)
  }
;;
