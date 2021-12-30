(* ocamlbuild -use-menhir test.byte *)

open Lexing

(*Compiler output file*)
let file = open_out "prog.asm"

(*Custom Std*)
let mystand_lib = open_in "Code/baselib.c"

(*For error messages*)
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
  (*Open the file to compile*)
  let f = open_in Sys.argv.(1) in
  (* Tokenize the file *)
  let buf = Lexing.from_channel f in
  try
    let parsed_stand_lib = Parser.prog Lexer.token (Lexing.from_channel mystand_lib) in
    (*Generate AST with type informations*)
    let parsed = Parser.prog Lexer.token buf in
    close_in f;
    close_in mystand_lib;
    (*Verify prog typing*)
    let ast = Semantics.analyze (parsed_stand_lib @ parsed) in
    (*Simplify String and duplicates also does a first safe deadcode cleanup*)
    let simpli_type, simpli_lbl = Simplifier.simplify ast in
    (*Generate Assembly Code from AST and does a second safe deadcode cleanup*)
    let asm = Compiler.compile simpli_type simpli_lbl in
    Mips.emit file asm
  with
  | Lexer.Error c ->
    err (Printf.sprintf "unrecognized char '%c'" c) (Lexing.lexeme_start_p buf)
  | Lexer.StrEndError -> err (Printf.sprintf "Close String") (Lexing.lexeme_start_p buf)
  | Lexer.ComEndError -> err (Printf.sprintf "Close Comment") (Lexing.lexeme_start_p buf)
  | Parser.Error -> err "syntax error" (Lexing.lexeme_start_p buf) (*Unrecognized Stuff*)
  | Semantics.Error (msg, pos) -> err msg pos
  | Ast.Error (msg, pos) -> err msg pos
;;
