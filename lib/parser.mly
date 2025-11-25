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
    { let (pos, _) : Lexing.position * Lexing.position = $loc in
      Var_decl ({ name; init }, pos) }
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
    { let (pos, _) : Lexing.position * Lexing.position = $loc in
      { name; params; body; pos } }
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
    { let (pos, _) : Lexing.position * Lexing.position = $loc in
      Return_stmt (e, pos) }
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
      Assign_expr ({lhs; rhs}, pos) }
  | expr = logic_or
    { expr }
  ;

logic_or:
  | lhs = logic_and; OR; rhs = logic_or
    { let (pos, _) : Lexing.position * Lexing.position = $loc in
      Or_expr (lhs, rhs, pos) }
  | expr = logic_and
    { expr }
  ;

logic_and:
  | lhs = equality; AND; rhs = logic_and
    { let (pos, _) : Lexing.position * Lexing.position = $loc in
      And_expr (lhs, rhs, pos) }
  | expr = equality
    { expr }
  ;

equality:
  | lhs = comparison; EQ; rhs = equality
    { let (pos, _) : Lexing.position * Lexing.position = $loc in
      Eq_expr (lhs, rhs, pos) }
  | lhs = comparison; NEQ; rhs = equality
    { let (pos, _) : Lexing.position * Lexing.position = $loc in
      Neq_expr (lhs, rhs, pos) }
  | expr = comparison
    { expr }
  ;

comparison:
  | lhs = term; LT; rhs = comparison
    { let (pos, _) : Lexing.position * Lexing.position = $loc in
      Lt_expr (lhs, rhs, pos) }
  | lhs = term; LEQ; rhs = comparison
    { let (pos, _) : Lexing.position * Lexing.position = $loc in
      Leq_expr (lhs, rhs, pos) }
  | lhs = term; GT; rhs = comparison
    { let (pos, _) : Lexing.position * Lexing.position = $loc in
      Gt_expr (lhs, rhs, pos) }
  | lhs = term; GEQ; rhs = comparison
    { let (pos, _) : Lexing.position * Lexing.position = $loc in
      Geq_expr (lhs, rhs, pos) }
  | expr = term
    { expr }
  ;

term:
  | lhs = factor; PLUS; rhs = term
    { let (pos, _) : Lexing.position * Lexing.position = $loc in
      Add_expr (lhs, rhs, pos) }
  | lhs = factor; MINUS; rhs = term
    { let (pos, _) : Lexing.position * Lexing.position = $loc in
      Sub_expr (lhs, rhs, pos) }
  | expr = factor
    { expr }
  ;

factor:
  | lhs = unary; TIMES; rhs = factor
    { let (pos, _) : Lexing.position * Lexing.position = $loc in
      Mult_expr (lhs, rhs, pos) }
  | lhs = unary; DIVIDE; rhs = factor
    { let (pos, _) : Lexing.position * Lexing.position = $loc in
      Div_expr (lhs, rhs, pos) }
  | expr = unary
    { expr }
  ;

unary:
  | BANG; e = unary
    { let (pos, _) : Lexing.position * Lexing.position = $loc in
      Neg_expr (e, pos) }
  | MINUS; e = unary
    { let (pos, _) : Lexing.position * Lexing.position = $loc in
      Minus_expr (e, pos) }
  | expr = call
    { let (pos, _) : Lexing.position * Lexing.position = $loc in
      Ast.Call_expr (expr, pos) }
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
    { let (pos, _) : Lexing.position * Lexing.position = $loc in
      Var_expr (id, pos) }
  | LEFT_PAREN; e = expr; RIGHT_PAREN
    { let (pos, _) : Lexing.position * Lexing.position = $loc in
      Expr_expr (e, pos) }
  | SUPER; DOT; id = IDENTIFIER
    { let (pos, _) : Lexing.position * Lexing.position = $loc in
      Ast.Super_expr (id, pos) }
