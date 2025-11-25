open! Core
open Lox

let do_lox ~env lexbuf =
  let ast = Util.parse_with_error lexbuf in
  match List.for_all ast ~f:Util.do_semant with
  | false -> env
  | true -> Util.do_interpret ~env ast

let rec repl ~l_num ~env () =
  eprintf "lox:> %!";
  match In_channel.input_line In_channel.stdin with
  | None -> ()
  | Some line ->
      let lexbuf = Lexing.from_string line in
      lexbuf.lex_curr_p <-
        { lexbuf.lex_curr_p with pos_fname = "-"; pos_lnum = l_num };
      let env = do_lox ~env lexbuf in
      repl ~l_num:(l_num + 1) ~env ()

let run_file ~env ~filename () =
  In_channel.with_file filename ~f:(fun f ->
      let lexbuf = Lexing.from_channel f in
      lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
      ignore @@ do_lox ~env lexbuf)

let dispatch ~filename =
  let env = Util.initial_env () in
  match filename with "-" -> repl ~l_num:1 ~env | _ -> run_file ~env ~filename

let () =
  Command.basic ~summary:"An interpreter for Lox"
    (let%map_open.Command filename =
       anon @@ maybe_with_default "-" ("file" %: string)
     in
     dispatch ~filename)
  |> Command_unix.run
