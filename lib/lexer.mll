{
    open Core
    open Parser

    exception SyntaxError of string
}

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

let alpha = ['a'-'z' 'A'-'Z' '_']
let digit = ['0'-'9']

let number = digit+ ('.' digit+)?
let identifier = alpha (alpha | digit)*

rule read =
  parse
  | white           { read lexbuf }
  | newline         { Lexing.new_line lexbuf; read lexbuf}
  | '('             { LEFT_PAREN }
  | ')'             { RIGHT_PAREN }
  | '{'             { LEFT_BRACE }
  | '}'             { RIGHT_BRACE }
  | ','             { COMMA }
  | '.'             { DOT }
  | '='             { ASSIGN }
  | ';'             { SEMICOLON }
  | '<'             { LT }
  | '>'             { GT }
  | '+'             { PLUS }
  | '-'             { MINUS }
  | '*'             { TIMES }
  | '/'             { DIVIDE }
  | '!'             { BANG }
  | "=="            { EQ }
  | "!="            { NEQ }
  | "<="            { LEQ }
  | ">="            { GEQ }
  | "class"         { CLASS }
  | "fun"           { FUN }
  | "var"           { VAR }
  | "for"           { FOR }
  | "if"            { IF }
  | "else"          { ELSE }
  | "print"         { PRINT }
  | "return"        { RETURN }
  | "while"         { WHILE }
  | "or"            { OR }
  | "and"           { AND }
  | "true"          { TRUE }
  | "false"         { FALSE }
  | "nil"           { NIL }
  | "this"          { THIS }
  | "super"         { SUPER }
  | "//"            { consume_comment lexbuf }
  | '"'             { read_string (Buffer.create 16) lexbuf }
  | number          { NUMBER (Float.of_string @@ Lexing.lexeme lexbuf) }
  | identifier      { IDENTIFIER (Lexing.lexeme lexbuf) }
  | _               { raise (SyntaxError ("Unexpected character: " ^ Lexing.lexeme lexbuf)) }
  | eof             { EOF }
and consume_comment =
  parse
  | newline         { Lexing.new_line lexbuf; read lexbuf }
  | _               { consume_comment lexbuf }
  | eof             { EOF }
and read_string buf =
  parse
  | '"'             { STRING (Buffer.contents buf) }
  | newline         { Lexing.new_line lexbuf;
                      Buffer.add_string buf (Lexing.lexeme lexbuf);
                      read_string buf lexbuf }
  | [^ '"']+        { Buffer.add_string buf (Lexing.lexeme lexbuf);
                      read_string buf lexbuf }
  | eof             { raise (SyntaxError ("Unterminated string")) }
