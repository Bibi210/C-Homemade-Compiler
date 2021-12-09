type reg =
  | V0
  | A0
  | SP
  | RA
  | T1
  | T0

type label = string

module Syscall = struct
  let print_int = 1
  let print_str = 4
  let read_int = 5
  let read_str = 8
  let sbrk = 9
end

type addr =
  | Lbl of label
  | Reg of reg
  | Mem of reg * int

type instr =
  | Jump_Lbl of label
  | Li of addr * int
  | La of addr * addr
  | Lw of addr * addr
  | Syscall
  | Jr of addr
  | Move of reg * reg
  | Jal of label
  | Sw of reg * addr
  | Addi of reg * reg * int
  | Add of reg * reg * reg
  | Mul of reg * reg * reg

type directive = Asciiz of string
type decl = label * directive

type asm =
  { text : instr list
  ; data : decl list
  }

let ps = Printf.sprintf (* alias raccourci *)

let fmt_reg = function
  | V0 -> "$v0"
  | A0 -> "$a0"
  | SP -> "$sp"
  | RA -> "$ra"
  | T1 -> "$t1"
  | T0 -> "$t0"
;;

let fmt_addr = function
  | Lbl t -> t
  | Reg t -> fmt_reg t
  | Mem (r, offset) -> ps "%d(%s)" offset (fmt_reg r)
;;

let fmt_instr = function
  | Li (r, i) -> ps "  li %s, %d" (fmt_addr r) i
  | La (r, addr) -> ps "  la %s, %s" (fmt_addr r) (fmt_addr addr)
  | Syscall -> ps "  syscall"
  | Jump_Lbl name -> ps "%s:" name
  | Lw (r, addr) -> ps "  lw %s %s" (fmt_addr r) (fmt_addr addr)
  | Jr lbl -> ps "  jr %s" (fmt_addr lbl)
  | Move (rd, rs) -> ps "  move %s, %s" (fmt_reg rd) (fmt_reg rs)
  | Jal lbl -> ps "  jal %s" lbl
  | Sw (r, addr) -> ps "  sw %s, %s" (fmt_reg r) (fmt_addr addr)
  | Addi (result, resv, to_add) ->
    ps "  addi %s, %s, %d" (fmt_reg result) (fmt_reg resv) to_add
  | Add (result, resv, to_add) ->
    ps "  add %s, %s, %s" (fmt_reg result) (fmt_reg resv) (fmt_reg to_add)
  | Mul (result, resv, multiplier) ->
    ps "  mul %s, %s, %s" (fmt_reg result) (fmt_reg resv) (fmt_reg multiplier)
;;

let fmt_dir = function
  | Asciiz s -> ps ".asciiz \"%s\"" s
;;

let emit oc asm =
  Printf.fprintf oc ".text\n.globl main\nmain:\n";
  List.iter (fun i -> Printf.fprintf oc "%s\n" (fmt_instr i)) asm.text;
  Printf.fprintf oc "\n.data\n";
  List.iter (fun (l, d) -> Printf.fprintf oc "%s: %s\n" l (fmt_dir d)) asm.data
;;
