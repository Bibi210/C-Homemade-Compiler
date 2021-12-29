%{
  open Ast.Syntax
  open Ast
  module Types_Map = Map.Make (String)
  let return_encount = ref false 
  let get_val x default = 
  match x with
  |None -> default
  |Some result -> result

 let typestoken = ref Types_Map.empty
  let get_type token pos = match Types_Map.find_opt token !typestoken with
  |None -> raise (Error (Printf.sprintf "Undefined type" , pos))
  |Some known_type -> known_type 

%}
%token <string>Lstring
%token <bool> Lbool
%token <int> Lint

%token Ldef_int Ldef_bool Ldef_string Ldef_void Ltypedef
%token <string> Lident Ltype

%token Lwhile Lfor Lif Lelse Ldo Lbreak Lcontinue Lreturn  Lc (* Lcase Ldefault  Lswitch *) Lgoto

%token Lend Lvirgule Lopar Lcpar Lsc Lobra_curl Lcbra_curl

%token Lassign

%token Linf Lsup
%token Let Lor
%token Leq Lnot
%token Lmult Ldiv Lmod
%token Ladd Lsous 

%right Lassign

%left Lor
%left Let
%left Leq
%left Linf Lsup

%left Ladd Lsous
%left Lmult Ldiv Lmod

%nonassoc Lnot


%start prog

%type <Ast.Syntax.prog> prog

%%

prog:
| b = list(def); Lend { b };

def:
|func_type = func_types; func_name = Lident;Lopar;args = separated_list(Lvirgule,pair (var_types,Lident) );Lcpar;body = block{
  Func { func_type = func_type
        ; func_name = func_name
        ; args = args
        ; pos = $startpos(func_name)
        ; block = if !return_encount then (return_encount := false; body) else 
        
        (body @  
        [Return 
          {expr =  
              Value { value = Void; pos = $endpos(body)}

          ;pos = $endpos(body)
          }])
        }
}
|Ltypedef;old_type = var_types;new_type = Ltype;Lsc {
  typestoken := Types_Map.add new_type (Custom_t(old_type)) !typestoken ;
 Typedef {pos = $startpos($1); new_type = Custom_t (old_type) ; old_type = old_type}
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
  return_encount := true;
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



decl:
|var_type = var_types;arguments = separated_nonempty_list(Lvirgule, expr){
  List.map 
  (fun a -> match a with
  |Assign syntax_assign -> [Decl{var_type = var_type
  ;var_name = 
  (match syntax_assign.var_name with 
  | Lvar x -> x 
  | Lderef x -> x )
  ;pos = syntax_assign.pos};Expr (Assign syntax_assign) ] 
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
| func_name = Lident;Lopar;argument = separated_list(Lvirgule, expr);Lcpar
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

(*Parsing Solo Operators*)
| op_name = solo_op; expr= expr {
  Call {name = op_name
  ; args = [expr]
  ; pos = $startpos(op_name)}
}


|var_recieve = var_ptr;Lassign; expr = expr{
  Assign {var_name = var_recieve;expr = expr;pos = $startpos($2)}
}


|Lopar;Lmult;var = Lident;Lcpar{
 Deref {var_name = var;pos = $startpos($2)}
}
|Lopar;Let;var = Lident;Lcpar{
 Addr {var_name = var;pos = $startpos($2)}
}

(*Parenthèse Proccesing *)
|Lopar ; expr = expr ; Lcpar {expr}

var_ptr:
|Lopar;Lmult;ptr = Lident;Lcpar{
  Lderef ptr
}
|var = Lident{
  Lvar var
}




var_types:
|Ldef_bool {Bool_t}
|Ldef_string {Str_t}
|Ldef_int {Int_t}
|defined_type = Ltype {get_type defined_type $startpos(defined_type)}
|pt_type = var_types;Lmult {Pointer_t (pt_type)}

%inline func_types:
|v_type = var_types {v_type}
|Ldef_void {Void_t}

%inline op:
|Ladd {"plus"}
|Lsous {"sous"}
|Lmult {"mult"}
|Lmod {"mod"}
|Ldiv {"div"}
|Lor;Lor {"or"}
|Leq {"eq"}
|Let;Let {"and"}
|Lsup {"sup"}
|Lsup;Lassign {"sup_or_equal"}
|Linf{"inf"}
|Linf;Lassign {"inf_or_equal"}
|Lnot;Lassign {"not_equal"}


%inline solo_op:
|Lnot {"not"}



%inline prog_val:
|nb = Lint {Base_Value.Int nb}
|bo = Lbool {Base_Value.Bool bo}
|str = Lstring {Base_Value.Str str}