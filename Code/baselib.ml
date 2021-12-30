open Ast
open Mips
module Env = Map.Make (String)

(* Native functions prototypes *)
let def_t =
  [ "_.mult", Func_t (Int_t, [ Int_t; Int_t ])
  ; "_.puts", Func_t (Void_t, [ Str_t ])
  ; "_.plus", Func_t (Int_t, [ Int_t; Int_t ])
  ; "_.puti", Func_t (Void_t, [ Int_t ])
  ; "_.sous", Func_t (Int_t, [ Int_t; Int_t ])
  ; "_.div", Func_t (Int_t, [ Int_t; Int_t ])
  ; "_.mod", Func_t (Int_t, [ Int_t; Int_t ])
  ; "_.or", Func_t (Bool_t, [ Bool_t; Bool_t ])
  ; "_.eq", Func_t (Bool_t, [ Int_t; Int_t ])
  ; "_.and", Func_t (Bool_t, [ Bool_t; Bool_t ])
  ; "_.inf", Func_t (Bool_t, [ Int_t; Int_t ])
  ]
;;

(* Native functions prototypes *)

let _types_ = Env.of_seq (List.to_seq def_t)

(*Assembly Code of native functions*)
let builtins =
  [ Jump_Lbl "_.puts"
  ; Lw (Reg A0, Mem (SP, 0))
  ; Li (Reg V0, Syscall.print_str)
  ; Syscall
  ; Jr RA
  ; Jump_Lbl "_.plus"
  ; Lw (Reg T0, Mem (SP, 0))
  ; Lw (Reg T1, Mem (SP, 4))
  ; Add (V0, T1, T0)
  ; Jr RA
  ; Jump_Lbl "_.sous"
  ; Lw (Reg T0, Mem (SP, 0))
  ; Lw (Reg T1, Mem (SP, 4))
  ; Sub (V0, T1, T0)
  ; Jr RA
  ; Jump_Lbl "_.mult"
  ; Lw (Reg T0, Mem (SP, 0))
  ; Lw (Reg T1, Mem (SP, 4))
  ; Mul (V0, T0, T1)
  ; Jr RA
  ; Jump_Lbl "_.puti"
  ; Lw (Reg A0, Mem (SP, 0))
  ; Li (Reg V0, Syscall.print_int)
  ; Syscall
  ; Jr RA
  ; Jump_Lbl "_.div"
  ; Lw (Reg T0, Mem (SP, 0))
  ; Lw (Reg T1, Mem (SP, 4))
  ; Div (V0, T1, T0)
  ; Jr RA
  ; Jump_Lbl "_.mod"
  ; Lw (Reg T0, Mem (SP, 0))
  ; Lw (Reg T1, Mem (SP, 4))
  ; Rem (V0, T1, T0)
  ; Jr RA
  ; Jump_Lbl "_.eq"
  ; Lw (Reg T0, Mem (SP, 0))
  ; Lw (Reg T1, Mem (SP, 4))
  ; Seq (V0, T1, T0)
  ; Jr RA
  ; Jump_Lbl "_.or"
  ; Lw (Reg T0, Mem (SP, 0))
  ; Lw (Reg T1, Mem (SP, 4))
  ; Or (V0, T1, T0)
  ; Jr RA
  ; Jump_Lbl "_.and"
  ; Lw (Reg T0, Mem (SP, 0))
  ; Lw (Reg T1, Mem (SP, 4))
  ; And (V0, T1, T0)
  ; Jr RA
  ; Jump_Lbl "_.inf"
  ; Lw (Reg T0, Mem (SP, 0))
  ; Lw (Reg T1, Mem (SP, 4))
  ; Slt (V0, T1, T0)
  ; Jr RA
  ]
;;
(*Assembly Code of native functions*)
