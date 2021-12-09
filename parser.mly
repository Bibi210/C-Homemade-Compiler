%{
  open Ast.Syntax
%}
%token <string>Lstring
%token <bool> Lbool
%token <int> Lint

%token <string> Lident

%token Lmult Ladd
%token Lend Lvirgule Lopar Lcpar

%left Ladd 
%left Lmult
%start prog

%type <Ast.Syntax.expr> prog

%%

prog:
| e = expr; Lend { e }
;

args: 
  | { [] }
  | hd = expr { [hd] }
  | hd = expr Lvirgule tail = args
    { hd :: tail }

expr:
|Lend {Value {value = Void ; pos = $startpos($1)}} (*Just For Now For Tests *)
| n = Lint {
  Value { value = Int n ; pos = $startpos(n) }
}
| boolean = Lbool {
  Value { value = Bool boolean ; pos = $startpos(boolean)}
}
| str = Lstring {
  Value { value = Str str ; pos = $startpos(str)}
}
| func_name = Lident;Lopar;argument = args;Lcpar
  {Call{name = func_name ; args = argument ; pos = $startpos(func_name)} }

| var_name = Lident{
  Var{name = var_name ; pos = $startpos(var_name)}
}

| expr_a = expr; Ladd; expr_b = expr {
  Call {name = "_plus_"
  ; args = [expr_a ; expr_b]
  ; pos = $startpos($2)}
}
| expr_a = expr; Lmult; expr_b = expr {
  Call {name = "_mult_"
  ; args = [expr_a ; expr_b]
  ; pos = $startpos($2)}
}
|Lopar ; expr_op = expr ; Lcpar {expr_op}

