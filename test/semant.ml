open! Core

let check_semant lexbuf =
  let ast = Lox.Parser.program Lox.Lexer.read lexbuf in
  List.iter ast ~f:(fun d ->
      let _ = Lox.Util.do_semant d in
      ())

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
  [%expect
    {|
    :2:7: Expected lvalue before '='.
    :3:7: Expected lvalue before '='.
    :4:7: Expected lvalue before '='.
    :5:7: Expected lvalue before '='.
    :6:7: Expected lvalue before '='.
    |}]

let%expect_test "reject calls with too many arguments" =
  In_channel.with_file "../test_programs/function/too_many_arguments.lox"
    ~f:(fun f ->
      let lexbuf = Lexing.from_channel f in
      check_semant lexbuf);
  [%expect {| :4:3: Can't have more than 255 arguments. |}]

let%expect_test "reject function declarations with too many parameters" =
  In_channel.with_file "../test_programs/function/too_many_parameters.lox"
    ~f:(fun f ->
      let lexbuf = Lexing.from_channel f in
      check_semant lexbuf);
  [%expect {| :2:5: Can't have more than 255 parameters. |}]

let%expect_test "reject non-empty return from a constructor" =
  let lexbuf =
    Lexing.from_string
      {|
    class Foo {
      init() {
          return 10;
      }
    }

    var f = Foo();
    |}
  in
  check_semant lexbuf;
  [%expect {| :4:11: Can't use a nonempty return from an initializer |}]

let%expect_test "allow empty return from a constructor" =
  let lexbuf =
    Lexing.from_string
      {|
    class Foo {
      init() {
          return;
      }
    }

    var f = Foo();
    |}
  in
  check_semant lexbuf;
  [%expect {| |}]
