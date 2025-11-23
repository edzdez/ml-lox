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

%token EOF

%start <Abt.program option> program
%%

program:
    | EOF       { None }
    ;
