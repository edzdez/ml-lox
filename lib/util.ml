open! Core

let print_position outx (pos : Lexing.position) =
  fprintf outx "%s:%d:%d" pos.pos_fname pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try Parser.program Lexer.read lexbuf with
  | Lexer.SyntaxError msg ->
      fprintf stderr "%a: %s\n%!" print_position lexbuf.lex_curr_p msg;
      []
  | Parser.Error state ->
      fprintf stderr "%a: %s%!" print_position lexbuf.lex_curr_p
        (Parser_messages.message state);
      []

let do_semant decl =
  try
    Semant.check_declaration decl;
    true
  with Semant.SemantError (pos, msg) ->
    fprintf stderr "%a: %s\n%!" print_position pos msg;
    false

let do_interpret ~env ast =
  try
    List.iter ast ~f:(fun decl -> Interpreter.execute_declaration env decl)
  with
  | Interpreter.EvalError (pos, msg) ->
      fprintf stderr "%a: %s\n%!" print_position pos msg
  | Environment.EnvError (pos, msg) ->
      fprintf stderr "%a: %s\n%!" print_position pos msg

let string_of_token (t : Parser.token) =
  match t with
  | Parser.WHILE -> "WHILE"
  | Parser.VAR -> "VAR"
  | Parser.TRUE -> "TRUE"
  | Parser.TIMES -> "TIMES"
  | Parser.THIS -> "THIS"
  | Parser.SUPER -> "SUPER"
  | Parser.STRING s -> sprintf "STRING[%s]" (String.escaped s)
  | Parser.SEMICOLON -> "SEMICOLON"
  | Parser.RIGHT_PAREN -> "LEFT_PAREN"
  | Parser.RIGHT_BRACE -> "RIGHT_PAREN"
  | Parser.RETURN -> "RETURN"
  | Parser.PRINT -> "PRINT"
  | Parser.PLUS -> "PLUS"
  | Parser.OR -> "OR"
  | Parser.NUMBER n -> sprintf "NUMBER[%f]" n
  | Parser.NIL -> "NIL"
  | Parser.NEQ -> "NEQ"
  | Parser.MINUS -> "MINUS"
  | Parser.LT -> "LT"
  | Parser.LEQ -> "LEQ"
  | Parser.LEFT_PAREN -> "LEFT_PAREN"
  | Parser.LEFT_BRACE -> "LEFT_BRACE"
  | Parser.IF -> "IF"
  | Parser.IDENTIFIER x -> sprintf "IDENTIFIER[%s]" x
  | Parser.GT -> "GT"
  | Parser.GEQ -> "GEQ"
  | Parser.FUN -> "FUN"
  | Parser.FOR -> "FOR"
  | Parser.FALSE -> "FALSE"
  | Parser.EQ -> "EQ"
  | Parser.EOF -> "EOF"
  | Parser.ELSE -> "ELSE"
  | Parser.DOT -> "DOT"
  | Parser.DIVIDE -> "DIVIDE"
  | Parser.COMMA -> "COMMA"
  | Parser.CLASS -> "CLASS"
  | Parser.BANG -> "BANG"
  | Parser.ASSIGN -> "ASSIGN"
  | Parser.AND -> "AND"
