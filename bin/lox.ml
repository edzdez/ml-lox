open! Core
open Lox

let do_lox lexbuf =
  let ast = Util.parse_with_error lexbuf in
  match List.for_all ast ~f:Util.do_semant with
  | false -> ()
  | true -> List.iter ast ~f:(Ast.print ~outx:stderr)

let rec repl () =
  eprintf "lox:> %!";
  match In_channel.input_line In_channel.stdin with
  | None -> ()
  | Some line ->
      let lexbuf = Lexing.from_string line in
      lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = "-" };
      do_lox lexbuf;
      repl ()

let run_file ~filename () =
  In_channel.with_file filename ~f:(fun f ->
      let lexbuf = Lexing.from_channel f in
      lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
      do_lox lexbuf)

let dispatch ~filename =
  match filename with "-" -> repl | _ -> run_file ~filename

let () =
  Command.basic ~summary:"An interpreter for Lox"
    (let%map_open.Command filename =
       anon @@ maybe_with_default "-" ("file" %: string)
     in
     dispatch ~filename)
  |> Command_unix.run
