%{
  open Ast.Syntax
  open Ast
  
%}
%token <string>Lstring
%token <bool> Lbool
%token <int> Lint

%token Ldef_int Ldef_bool Ldef_string
%token <string> Lident

%token Lwhile Lif Lelse

%token Lend Lvirgule Lopar Lcpar Lsc Lobra_curl Lcbra_curl

%token Lassign

%token Lmult Ldiv Lmod
%token Ladd Lsous 

%right Lassign

%left Ladd Lsous
%left Lmult


%start prog

%type <Ast.Syntax.block> prog

%%

prog:
| b = block; Lend { b };

block:
|Lobra_curl; instr_ls = list(instr) ; Lcbra_curl{
  List.flatten instr_ls
}
| ist = instr{ (*Provoque Grammaire ambigue pk ?*)
  ist
};

instr:
| Lsc {[]}
| expr = expr;Lsc;{
  [Expr expr]
}
| decl_ls = flatten(decl);Lsc{
  decl_ls
}
|Lwhile ; cond = expr; to_exec = block{
  [While {cond = cond;pos = $startpos($1);block = to_exec}]
}
|Lif ; cond = expr; block_true = block;opt_else = option(pair(Lelse,block)){
  [If {cond = cond;pos = $startpos($1);block_true = block_true
  ; block_false = (match opt_else with 
  | None -> []
  | Some(_,b) -> b)
  }]
}
;

decl:
|var_type = var_types;arguments = separated_nonempty_list(Lvirgule, expr){
  List.map 
  (fun a -> match a with
  |Assign syntax_assign -> [Decl{var_type = var_type;var_name = syntax_assign.var_name;pos = syntax_assign.pos};Expr (Assign syntax_assign) ] 
  |Var syntax_var -> [Decl{var_type = var_type;var_name = syntax_var.name;pos = syntax_var.pos}]
  |x -> raise (Error (Printf.sprintf "Not Valid Init" , $endpos(arguments)))
  )
  arguments 
}
;

expr:
(*Parsing Values*)
| n = prog_val { 
  Value { value = n ; pos = $startpos(n) }
}
(*Parsing Functions*)
| func_name = Lident;Lopar;argument = separated_nonempty_list(Lvirgule, expr);Lcpar
  {Call{name = func_name ; args = argument ; pos = $startpos(func_name)} }
(*Parsing Variables*)
| var_name = Lident{
  Var{name = var_name ; pos = $startpos(var_name)}
}
(*Parsing Basic Operators*)
| expr_a = expr; op_name = op; expr_b = expr {
  Call {name = op_name
  ; args = [expr_a ; expr_b]
  ; pos = $startpos(op_name)}
}
|var_recieve = Lident;Lassign; expr = expr{ (*Provoque Grammaire ambigue a OP pk ?*)
  Assign {var_name = var_recieve;expr = expr;pos = $startpos($2)}
}
(*Parenthèse Proccesing *)
|Lopar ; expr = expr ; Lcpar {expr}



%inline var_types:
|Ldef_bool {Bool_t}
|Ldef_string {Str_t}
|Ldef_int {Int_t}

%inline op:
|Ladd {"_plus_"}
|Lsous {"_sous_"}
|Lmult {"_mult_"}
|Lmod {"_mod_"}
|Ldiv {"_div_"}

%inline prog_val:
|nb = Lint {Base_Value.Int nb}
|bo = Lbool {Base_Value.Bool bo}
|str = Lstring {Base_Value.Str str}