{
  (*open Lexing*)
  open Parser

  exception Error of char
  exception StrEndError
  exception ComEndError
}

let alpha = ['a'-'z' 'A'-'Z']
let num = ['0'-'9']
let identifier = alpha (alpha | num | '-' | '_')*
rule token = parse
| eof             { Lend }
| [ ' ' '\t' ]    { token lexbuf }
| '\n'            { Lexing.new_line lexbuf; token lexbuf }
| "true" as boolean          {Lbool (bool_of_string boolean) }
| "false" as boolean          {Lbool (bool_of_string boolean) }
| '(' {Lopar}
| ')' {Lcpar}
| ',' {Lvirgule}
| '*' {Lmult}
| '+' {Ladd}
| "//" {line_comment lexbuf}
| "/*" {multi_line_comment lexbuf}
| '"'        { Lstring (String.of_seq (List.to_seq (string_read lexbuf))) }
| num+ as n       { Lint (int_of_string n) }
|identifier+ as read_ident {Lident read_ident}
| _ as c          { raise (Error c) }

and string_read = parse
| eof { raise (StrEndError) }
| '"' { [] }
| "\\n" { '\n' :: (string_read lexbuf) } 
| "\\t" { '\t' :: (string_read lexbuf) }
| "\\\\" { '\\' :: (string_read lexbuf) } 
| _ as c { c :: (string_read lexbuf) }

and line_comment = parse
| eof  { Lend }
| '\n' { Lexing.new_line lexbuf; token lexbuf }
| _    { line_comment lexbuf }

and multi_line_comment = parse
| eof  { raise (ComEndError) }
| "*/" { token lexbuf }
| _    { multi_line_comment lexbuf }