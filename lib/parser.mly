%token <float> NUMBER
%token <string> STRING
%token <string> IDENTIFIER

%token CLASS
%token FUN
%token VAR
%token FOR
%token IF
%token ELSE
%token PRINT
%token RETURN
%token WHILE
%token OR
%token AND
%token TRUE
%token FALSE
%token NIL
%token THIS
%token SUPER

%token COMMA
%token DOT
%token ASSIGN
%token SEMICOLON
%token LT
%token GT
%token PLUS
%token MINUS
%token TIMES
%token DIVIDE
%token BANG

%token EQ
%token NEQ
%token LEQ
%token GEQ

%token LEFT_PAREN
%token RIGHT_PAREN
%token LEFT_BRACE
%token RIGHT_BRACE

%nonassoc NO_ELSE
%nonassoc ELSE

%token EOF

%type <Ast.declaration> declaration
%type <string option> inherits
%type <Ast.func list> funcs
%type <Ast.func> func
%type <string list> parameters
%type <Ast.expr option> var_init
%type <Ast.statement> statement
%type <Ast.for_init_t> for_init
%type <Ast.expr option> for_cond
%type <Ast.expr option> for_step
%type <Ast.expr option> return_expr
%type <Ast.declaration list> block_body
%type <Ast.expr> expr
%type <Ast.expr> assignment
%type <Ast.expr> logic_or
%type <Ast.expr> logic_and
%type <Ast.expr> equality
%type <Ast.expr> comparison
%type <Ast.expr> term
%type <Ast.expr> factor
%type <Ast.expr> unary
%type <Ast.call_expr> call
%type <Ast.call_t list> call_calls
%type <Ast.expr list> arguments
%type <Ast.atom_expr> primary
%start <Ast.declaration list> program

%%

program:
  | EOF 
    { [] }
  | hd = declaration; tl = program
    { hd :: tl }
  ;

declaration:
  | CLASS; name = IDENTIFIER; parent = inherits; body = funcs
    { Class_decl { name; parent; body } }
  | FUN; f = func
    { Func_decl f }
  | VAR; name = IDENTIFIER; init = var_init;
    { Var_decl { name; init } }
  | stmt = statement
    { Stmt_decl stmt }
  ;

inherits:
  | LEFT_BRACE
    { None }
  | LT; parent = IDENTIFIER; LEFT_BRACE
    { Some parent }
  ;

funcs:
  | RIGHT_BRACE
    { [] }
  | f = func; fs = funcs
    { f :: fs }
  ;

func:
  | name = IDENTIFIER; LEFT_PAREN; params = parameters; LEFT_BRACE; body = block_body
    { { name; params; body} }
  ;

parameters:
  | RIGHT_PAREN
    { [] }
  | id = IDENTIFIER; RIGHT_PAREN
    { [id] }
  | hd = IDENTIFIER; COMMA; tl = parameters
    { hd :: tl }

var_init:
  | SEMICOLON
    { None }
  | ASSIGN; e = expr; SEMICOLON
    { Some e }
  ;

statement:
  | e = expr; SEMICOLON
    { Expr_stmt e }
  | FOR; LEFT_PAREN; init = for_init; cond = for_cond; step = for_step; body = statement
    { For_stmt { init; cond; step; body } }
  | IF; LEFT_PAREN; cond = expr; RIGHT_PAREN; consequent = statement
    { If_stmt { cond; consequent; alternative = None } } %prec NO_ELSE
  | IF; LEFT_PAREN; cond = expr; RIGHT_PAREN; consequent = statement; ELSE; alternative = statement
    { If_stmt { cond; consequent; alternative = Some alternative } }
  | PRINT; e = expr; SEMICOLON
    { Print_stmt e }
  | RETURN; e = return_expr
    { Return_stmt e }
  | WHILE; LEFT_PAREN; cond = expr; RIGHT_PAREN; body = statement
    { While_stmt { cond; body } }
  | LEFT_BRACE; body = block_body
    { Ast.Block_stmt body }
  ;

for_init:
  | SEMICOLON
    { None }
  | VAR; name = IDENTIFIER; init = var_init
    { Decl { name; init } }
  | e = expr; SEMICOLON
    { Expr e }
  ;

for_cond:
  | SEMICOLON
    { None }
  | e = expr; SEMICOLON
    { Some e }
  ;

for_step:
  | RIGHT_PAREN
    { None }
  | e = expr; RIGHT_PAREN
    { Some e }
  ;

return_expr:
  | SEMICOLON
    { None }
  | e = expr; SEMICOLON
    { Some e }
  ;

block_body:
  | RIGHT_BRACE
    { [] }
  | hd = declaration; tl = block_body
    { hd :: tl }
  ;

expr:
  | expr = assignment
    { expr }
  ;

assignment:
  | lhs = call; ASSIGN; rhs = assignment
    { let (pos, _) : Lexing.position * Lexing.position = $loc in
      Assign_expr ({lhs; rhs}, (pos.pos_fname, pos.pos_lnum, pos.pos_cnum)) }
  | expr = logic_or
    { expr }
  ;

logic_or:
  | lhs = logic_and; OR; rhs = logic_or
    { Or_expr (lhs, rhs) }
  | expr = logic_and
    { expr }
  ;

logic_and:
  | lhs = equality; AND; rhs = logic_and
    { And_expr (lhs, rhs) }
  | expr = equality
    { expr }
  ;

equality:
  | lhs = comparison; EQ; rhs = equality
    { Eq_expr (lhs, rhs) }
  | lhs = comparison; NEQ; rhs = equality
    { Neq_expr (lhs, rhs) }
  | expr = comparison
    { expr }
  ;

comparison:
  | lhs = term; LT; rhs = comparison
    { Lt_expr (lhs, rhs) }
  | lhs = term; LEQ; rhs = comparison
    { Leq_expr (lhs, rhs) }
  | lhs = term; GT; rhs = comparison
    { Gt_expr (lhs, rhs) }
  | lhs = term; GEQ; rhs = comparison
    { Geq_expr (lhs, rhs) }
  | expr = term
    { expr }
  ;

term:
  | lhs = factor; PLUS; rhs = term
    { Add_expr (lhs, rhs) }
  | lhs = factor; MINUS; rhs = term
    { Sub_expr (lhs, rhs) }
  | expr = factor
    { expr }
  ;

factor:
  | lhs = unary; TIMES; rhs = factor
    { Mult_expr (lhs, rhs) }
  | lhs = unary; DIVIDE; rhs = factor
    { Div_expr (lhs, rhs) }
  | expr = unary
    { expr }
  ;

unary:
  | BANG; e = unary
    { Neg_expr e }
  | MINUS; e = unary
    { Minus_expr e }
  | expr = call
    { Ast.Call_expr expr }
  ;

call:
  | primary = primary
    { { primary; calls = [] } }
  | primary = primary; calls = call_calls
    { { primary; calls } }
  ;

call_calls:
  | LEFT_PAREN; args = arguments
    { [Call args] }
  | LEFT_PAREN; args = arguments; tl = call_calls
    { Call args :: tl }
  | DOT; id = IDENTIFIER
    { [Member id] }
  | DOT; id = IDENTIFIER; tl = call_calls
    { Member id :: tl }
  ;

arguments:
  | RIGHT_PAREN
    { [] }
  | e = expr; RIGHT_PAREN
    { [e] }
  | e = expr; COMMA; tl = arguments
    { e :: tl }
  ;

primary:
  | TRUE
    { Bool_expr true }
  | FALSE
    { Bool_expr false }
  | NIL
    { Nil_expr }
  | THIS
    { This_expr }
  | n = NUMBER
    { Number_expr n }
  | s = STRING
    { String_expr s }
  | id = IDENTIFIER
    { Var_expr id }
  | LEFT_PAREN; e = expr; RIGHT_PAREN
    { Expr_expr e }
  | SUPER; DOT; id = IDENTIFIER
    { Ast.Super_expr id }
