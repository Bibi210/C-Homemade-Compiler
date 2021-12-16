type ident = string

exception Error of string * Lexing.position

type prog_type =
  | Bool_t
  | Int_t
  | Str_t
  | Void_t
  | Func_t of prog_type * prog_type list
(*| Var_t of prog_type * bool*)

module type Parameters = sig
  type value
end

module Base_Value = struct
  type value =
    | Bool of bool
    | Int of int
    | Str of string
    | Void
end

module Simplify_Value = struct
  type value =
    | Bool of bool
    | Int of int
    | Data of string
    | Void
end

module IR (P : Parameters) = struct
  type expr =
    | Value of P.value
    | Call of ident * expr list
    | Var of ident
    | Assign of ident * expr

  type instr =
    | Expr of expr
    | Decl of ident
    | While of expr * block
    | If of expr * block * block

  and block = instr list
end

module Base_IR = IR (Base_Value)
module Simplify_IR = IR (Simplify_Value)

module Syntax = struct
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
        { var_name : ident
        ; expr : expr
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
        ; block : block
        ; pos : Lexing.position
        }
    | If of
        { cond : expr
        ; block_true : block
        ; block_false : block
        ; pos : Lexing.position
        }

  and block = instr list
end
