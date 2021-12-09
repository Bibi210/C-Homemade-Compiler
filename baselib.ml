open Ast.Base_Value
open Mips
module Env = Map.Make (String)

let def_t =
  [ "_mult_", Func_t (Int_t, [ Int_t; Int_t ])
  ; "puts", Func_t (Void_t, [ Str_t ])
  ; "_plus_", Func_t (Int_t, [ Int_t; Int_t ])
  ; "puti", Func_t (Void_t, [ Int_t ])
  ]
;;

let _types_ = Env.of_seq (List.to_seq def_t)

let builtins =
  [ Jump_Lbl "puts"
  ; Lw (Reg A0, Mem (SP, 0))
  ; Li (Reg V0, Syscall.print_str)
  ; Syscall
  ; Jr (Reg RA)
  ; Jump_Lbl "_plus_"
  ; Lw (Reg T0, Mem (SP, 0))
  ; Lw (Reg T1, Mem (SP, 4))
  ; Add (V0, T0, T1)
  ; Jr (Reg RA)
  ; Jump_Lbl "_mult_"
  ; Lw (Reg T0, Mem (SP, 0))
  ; Lw (Reg T1, Mem (SP, 4))
  ; Mul (V0, T0, T1)
  ; Jr (Reg RA)
  ; Jump_Lbl "puti"
  ; Lw (Reg A0, Mem (SP, 0))
  ; Li (Reg V0, Syscall.print_int)
  ; Syscall
  ; Jr (Reg RA)
  ]
;;
