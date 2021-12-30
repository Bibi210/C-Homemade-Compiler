type ident = string

exception Error of string * Lexing.position

let native_func = "_."

(* Possible Types *)
type prog_type =
  | Bool_t
  | Int_t
  | Str_t
  | Void_t
  | Func_t of prog_type * prog_type list
  | Var_t of prog_type * bool
  | Pointer_t of prog_type
  | Custom_t of prog_type

(* Different Links Between Types *)
type typelinks =
  | Cast of prog_type
  | Derived of prog_type
  | Primitive_t

module type Parameters = sig
  type value
end

(*Before Simplifier*)
module Base_Value = struct
  type value =
    | Bool of bool
    | Int of int
    | Str of string
    | Void
end

(*After Simplifier*)
module Simplify_Value = struct
  type value =
    | Bool of bool
    | Int of int
    | Data of string
    | Void
end

module IR (P : Parameters) = struct
  (*Assign Left Value Types *)
  type lvalues =
    | Lderef of ident
    | Lvar of ident

  type expr =
    | Value of P.value
    | Call of ident * expr list
    | Var of ident
    | Assign of lvalues * expr
    | Deref of ident
    | Addr of ident

  type instr =
    | Expr of expr (*Cause an expr is an instruction*)
    | Decl of ident
    | While of expr * instr * bool
    | For of expr * expr * instr
    | If of expr * instr * instr
    | Break
    | Continue
    | NestedBlock of block (* Cause a block is also an instruction *)
    | Return of expr
    | Goto of ident
    | Label of ident
    | None
  (* Allow To Delete Dead Code *)

  and block = instr list

  type def = Func of ident * ident list * block
  type prog = def list
end

(*Before Simplifier*)
module Base_IR = IR (Base_Value)

(*After Simplifier*)
module Simplify_IR = IR (Simplify_Value)

module Syntax = struct
  (*Assign Left Value Types *)
  type lvalues =
    | Lderef of ident
    | Lvar of ident

  type expr =
    | Value of
        { value : Base_Value.value
        ; pos : Lexing.position
        }
    | Call of
        { name : ident
        ; args : expr list
        ; pos : Lexing.position
        }
    | Var of
        { name : ident
        ; pos : Lexing.position
        }
    | Assign of
        { var_name : lvalues
        ; expr : expr
        ; pos : Lexing.position
        }
    | Deref of
        { var_name : ident
        ; pos : Lexing.position
        }
    | Addr of
        { var_name : ident
        ; pos : Lexing.position
        }

  type instr =
    | Expr of expr
    | Decl of
        { var_name : ident
        ; pos : Lexing.position
        ; var_type : prog_type
        }
    | While of
        { cond : expr
        ; block : instr
        ; do_mode : bool
        ; pos : Lexing.position
        }
    | If of
        { cond : expr
        ; block_true : instr
        ; block_false : instr
        ; pos : Lexing.position
        }
    | For of
        { cond : expr
        ; incre : expr
        ; block : instr
        ; pos : Lexing.position
        }
    | Return of
        { expr : expr
        ; pos : Lexing.position
        }
    | Break of Lexing.position
    | Continue of Lexing.position
    | NestedBlock of block (*Dont need pos cause can't generate from itf *)
    | Label of
        { lbl : ident
        ; pos : Lexing.position
        }
    | Goto of
        { lbl : ident
        ; pos : Lexing.position
        }

  and block = instr list

  type def =
    | Func of
        { func_type : prog_type
        ; func_name : ident
        ; args : (prog_type * ident) list
        ; block : block
        ; pos : Lexing.position
        }
    | Typedef of
        { (*Only Exist for Sementics *)
          pos : Lexing.position
        ; new_type : prog_type
        ; old_type : prog_type
        }

  type prog = def list
end
