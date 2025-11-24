open! Core
open Lox

let parse_with_error lexbuf =
  let print_position outx (lexbuf : Lexing.lexbuf) =
    let pos = lexbuf.lex_curr_p in
    fprintf outx "%s:%d:%d" pos.pos_fname pos.pos_lnum
      (pos.pos_cnum - pos.pos_bol + 1)
  in
  try Parser.program Lexer.read lexbuf with
  | Lexer.SyntaxError msg ->
      fprintf stderr "%a: %s\n%!" print_position lexbuf msg;
      []
  | Parser.Error ->
      fprintf stderr "%a: Parse error\n%!" print_position lexbuf;
      []

let repl () = ()

let run_file ~filename () =
  In_channel.with_file filename ~f:(fun f ->
      let lexbuf = Lexing.from_channel f in
      lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
      let ast = parse_with_error lexbuf in
      List.iter ast ~f:Ast.print)

let dispatch ~filename =
  match filename with "-" -> repl | _ -> run_file ~filename

let () =
  Command.basic ~summary:"An interpreter for Lox"
    (let%map_open.Command filename =
       anon @@ maybe_with_default "-" ("file" %: string)
     in
     dispatch ~filename)
  |> Command_unix.run
