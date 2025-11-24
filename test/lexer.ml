open! Core

let rec print_tokens lexbuf =
  try
    let token = Lox.Lexer.read lexbuf in
    printf "%s " (Lox.Util.string_of_token token);
    match token with Lox.Parser.EOF -> () | _ -> print_tokens lexbuf
  with Lox.Lexer.SyntaxError s -> printf "\n%s\n" s

let%expect_test "emits EOF at end of file" =
  In_channel.with_file "../test_programs/empty_file.lox" ~f:(fun f ->
      let lexbuf = Lexing.from_channel f in
      print_tokens lexbuf;
      [%expect {| EOF |}])

let%expect_test "unexpected character error" =
  In_channel.with_file "../test_programs/unexpected_character.lox" ~f:(fun f ->
      let lexbuf = Lexing.from_channel f in
      print_tokens lexbuf;
      [%expect
        {|
        IDENTIFIER[foo] LEFT_PAREN IDENTIFIER[a]
        Unexpected character: |.
        |}])

let%expect_test "lexes identifiers" =
  In_channel.with_file "../test_programs/scanning/identifiers.lox" ~f:(fun f ->
      let lexbuf = Lexing.from_channel f in
      print_tokens lexbuf;
      [%expect
        {| IDENTIFIER[andy] IDENTIFIER[formless] IDENTIFIER[fo] IDENTIFIER[_] IDENTIFIER[_123] IDENTIFIER[_abc] IDENTIFIER[ab123] IDENTIFIER[abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_] EOF |}])

let%expect_test "lexes numbers" =
  In_channel.with_file "../test_programs/scanning/numbers.lox" ~f:(fun f ->
      let lexbuf = Lexing.from_channel f in
      print_tokens lexbuf;
      [%expect
        {| NUMBER[123.000000] NUMBER[123.456000] DOT NUMBER[456.000000] NUMBER[123.000000] DOT EOF |}])

let%expect_test "lexes strings" =
  In_channel.with_file "../test_programs/scanning/strings.lox" ~f:(fun f ->
      let lexbuf = Lexing.from_channel f in
      print_tokens lexbuf;
      [%expect {| STRING[] STRING[string] EOF |}])

let%expect_test "lexes keywords" =
  In_channel.with_file "../test_programs/scanning/keywords.lox" ~f:(fun f ->
      let lexbuf = Lexing.from_channel f in
      print_tokens lexbuf;
      [%expect
        {| AND CLASS ELSE FALSE FOR FUN IF NIL OR RETURN SUPER THIS TRUE VAR WHILE EOF |}])

let%expect_test "ignores whitespace" =
  In_channel.with_file "../test_programs/scanning/whitespace.lox" ~f:(fun f ->
      let lexbuf = Lexing.from_channel f in
      print_tokens lexbuf;
      [%expect
        {| IDENTIFIER[space] IDENTIFIER[tabs] IDENTIFIER[newlines] IDENTIFIER[end] EOF |}])

let%expect_test "lexes punctuators" =
  In_channel.with_file "../test_programs/scanning/punctuators.lox" ~f:(fun f ->
      let lexbuf = Lexing.from_channel f in
      print_tokens lexbuf;
      [%expect
        {| LEFT_PAREN LEFT_PAREN LEFT_BRACE RIGHT_PAREN SEMICOLON COMMA PLUS MINUS TIMES NEQ EQ LEQ GEQ NEQ LT GT DIVIDE DOT EOF |}])

let%expect_test "behaves with comments" =
  List.iter
    ~f:(fun file ->
      In_channel.with_file file ~f:(fun f ->
          let lexbuf = Lexing.from_channel f in
          print_tokens lexbuf;
          printf "\n"))
    [
      "../test_programs/comments/line_at_eof.lox";
      "../test_programs/comments/only_line_comment.lox";
      "../test_programs/comments/only_line_comment_and_line.lox";
      "../test_programs/comments/unicode.lox";
    ];
  [%expect
    {|
    PRINT STRING[ok] SEMICOLON EOF
    EOF
    EOF
    PRINT STRING[ok] SEMICOLON EOF
    |}]

let%expect_test "multiline strings" =
  let lexbuf =
    Lexing.from_string {|
        "hi
        there
        whoa"
    |}
  in
  print_tokens lexbuf;
  [%expect {| STRING[hi\n        there\n        whoa] EOF |}]
