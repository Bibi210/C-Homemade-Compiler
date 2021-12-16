(* ocamlbuild -use-menhir test.byte *)

open Lexing
open Ast
open Simplifier

let file = open_out "prog.asm"

let err msg pos =
  Printf.eprintf
    "Error on line %d col %d: %s.\n"
    pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol)
    msg;
  exit 1
;;

let () =
  if Array.length Sys.argv != 2
  then (
    Printf.eprintf "Usage: %s <file>\n" Sys.argv.(0);
    exit 1);
  let f = open_in Sys.argv.(1) in
  let buf = Lexing.from_channel f in
  try
    let parsed = Parser.prog Lexer.token buf in
    close_in f;
    let ast = Semantics.analyze parsed in
    let simpli_type, simpli_lbl = Simplifier.simplify ast in
    let asm = Compiler.compile simpli_type simpli_lbl in
    Mips.emit file asm
  with
  | Lexer.Error c ->
    err (Printf.sprintf "unrecognized char '%c'" c) (Lexing.lexeme_start_p buf)
  | Lexer.StrEndError -> err (Printf.sprintf "Close String") (Lexing.lexeme_start_p buf)
  | Lexer.ComEndError -> err (Printf.sprintf "Close Comment") (Lexing.lexeme_start_p buf)
  | Parser.Error -> err "syntax error" (Lexing.lexeme_start_p buf)
  | Semantics.Error (msg, pos) -> err msg pos
  | Ast.Error (msg, pos) -> err msg pos
;;
