%{
  open Ast.Syntax
  open Ast
  let get_val x default = 
  match x with
  |None -> default
  |Some result -> result

%}
%token <string>Lstring
%token <bool> Lbool
%token <int> Lint

%token Ldef_int Ldef_bool Ldef_string Ldef_void
%token <string> Lident

%token Lwhile Lfor Lif Lelse Ldo Lbreak Lcontinue Lreturn Lswitch Lc Lcase Ldefault Lgoto

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

func_def:
|func_type = func_types; func_name = Lident;Lopar;args = separated_list(Lvirgule, expr);Lcpar;body = block{

}
;
block:
|Lobra_curl; instr_ls = list(instr) ; Lcbra_curl{
  List.flatten instr_ls
};

instr:
| Lsc {[]}
| expr = expr;Lsc;{
  [Expr expr]
}
| decl_ls = flatten(decl);Lsc{
  decl_ls
}
|Lreturn ;opt_expr = option(expr);Lsc{
  let expr = get_val opt_expr (Value {value = Void;pos = $startpos(opt_expr)}) in
  [Return {expr = expr;pos = $startpos($1)}]
}
|Lwhile ; cond = expr; to_exec = instr{
  [While {cond = cond;pos = $startpos($1);block = NestedBlock to_exec;do_mode = false;}]
}
|Ldo;to_exec = instr;Lwhile;cond = expr;Lsc{
  [While {cond = cond;pos = $startpos($1);block = NestedBlock to_exec;do_mode = true;}]
}
|Lfor;Lopar ;dcl = instr ; opt_cond = option(expr); Lsc;opt_incre = option(expr); Lcpar; to_exec = instr{
  let cond = get_val opt_cond (Value {value = Bool true;pos = $startpos(opt_cond)}) in
  let incre = get_val opt_incre (Value {value = Void;pos = $startpos(opt_cond)}) in
  [NestedBlock (dcl @ [For {cond = cond;pos = $startpos($1);incre = incre ; block = NestedBlock to_exec}])]
}
|Lbreak{
  [Break $startpos($1)]
}
|Lcontinue{
  [Continue $startpos($1)]
}
|Lif ; cond = expr; block_true = instr;opt_else = option(pair(Lelse,instr)){
  [If {cond = cond;pos = $startpos($1);block_true = NestedBlock block_true
  ; block_false = (match opt_else with 
  | None ->NestedBlock []
  | Some(_,b) -> NestedBlock b)
  }]
}
|nested_block = block{
  [NestedBlock nested_block]
}
|lbl = Lident;Lc{
  [Label {lbl = lbl;pos = $startpos(lbl)}]
}
|Lgoto;lbl = Lident;Lsc{
  [Goto {lbl = lbl;pos = $startpos(lbl)}]
}
/* |Lswitch; expr ;Lobra_curl;pair(option(list(instr)),option(list(case)));Lcbra_curl{
  []
} 
;

 case:
|Lcase; cond = expr ; Lc ;opt_b =  option(list(instr)) {
  let b = get_val opt_b [] in
   cond,b
}
|Ldefault ; Lc ;opt_b =  option(list(instr)) {
  let b = get_val opt_b [] in
   Value {value = Bool true;pos = $startpos($1)},b
}  */


decl:
|var_type = var_types;arguments = separated_nonempty_list(Lvirgule, expr){
  List.map 
  (fun a -> match a with
  |Assign syntax_assign -> [Decl{var_type = var_type;var_name = syntax_assign.var_name;pos = syntax_assign.pos};Expr (Assign syntax_assign) ] 
  |Var syntax_var -> [Decl{var_type = var_type;var_name = syntax_var.name;pos = syntax_var.pos}]
  |_ -> raise (Error (Printf.sprintf "Not Valid Init" , $endpos(arguments)))
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
  Call {name = native_func^op_name
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

%inline func_types:
|v_type = var_types {v_type}
|Ldef_void {Void_t}

%inline op:
|Ladd {"plus"}
|Lsous {"sous"}
|Lmult {"mult"}
|Lmod {"mod"}
|Ldiv {"div"}

%inline prog_val:
|nb = Lint {Base_Value.Int nb}
|bo = Lbool {Base_Value.Bool bo}
|str = Lstring {Base_Value.Str str}