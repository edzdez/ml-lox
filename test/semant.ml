open! Core

let check_semant lexbuf =
  let ast = Lox.Parser.program Lox.Lexer.read lexbuf in
  List.iter ast ~f:(fun d ->
      try Lox.Semant.check_declaration d with Failure s -> printf "%s\n" s)

let%expect_test "allow assignments to lvalues" =
  let lexbuf =
    Lexing.from_string
      {|
      a = b;
      a.foo().c = b;
      32.name().bar.baz = 10;
    |}
  in
  check_semant lexbuf;
  [%expect {||}]

let%expect_test "reject assignments to non-lvalues" =
  let lexbuf =
    Lexing.from_string
      {|
      42 = b;
      a.foo() = b;
      nil = b;
      this = 10;
      a.b.foo().c.d() = b;
    |}
  in
  check_semant lexbuf;
  [%expect {|
    :2:7: Semantic error: left-hand side of `=` is not an lvalue
    :3:21: Semantic error: left-hand side of `=` is not an lvalue
    :4:40: Semantic error: left-hand side of `=` is not an lvalue
    :5:55: Semantic error: left-hand side of `=` is not an lvalue
    :6:72: Semantic error: left-hand side of `=` is not an lvalue
    |}]
