open! Core

let interpret lexbuf =
  let ast = Lox.Parser.program Lox.Lexer.read lexbuf in
  Lox.Util.do_interpret ast

let%expect_test "works with basic arithmetic expressions" =
  let lexbuf =
    Lexing.from_string
      {|
        1 + 2;
        "Hello, " + "world!";
        10 / 3;
        6 - 4;
        4 * 1;
    |}
  in
  interpret lexbuf;
  [%expect
    {|
    (Number 3)
    (String "Hello, world!")
    (Number 3.3333333333333335)
    (Number 2)
    (Number 4)
    |}]

let%expect_test "works with basic comparison expressions" =
  let lexbuf =
    Lexing.from_string
      {|
        1 < 2;
        2 < 2;
        2 <= 2;
        3 <= 2;
        1 > 2;
        2 > 2;
        2 >= 2;
        3 >= 2;
        nil == nil;
        5 == 4;
        "hi" == "hi";
        "hi" == "bye";
        5 == true;
        nil != nil;
        5 != 4;
        "hi" != "hi";
        "hi" != "bye";
        5 != true;
    |}
  in
  interpret lexbuf;
  [%expect
    {|
    (Bool true)
    (Bool false)
    (Bool true)
    (Bool false)
    (Bool false)
    (Bool false)
    (Bool true)
    (Bool true)
    (Bool true)
    (Bool false)
    (Bool true)
    (Bool false)
    (Bool false)
    (Bool false)
    (Bool true)
    (Bool false)
    (Bool true)
    (Bool true)
    |}]

let%expect_test "runtime error on type mismatches" =
  List.iter [ {|"hi" < 2;|}; {|1 > true;|}; {|4 + nil; |}; {|5 + "hi";|} ]
    ~f:(fun s ->
      let lexbuf = Lexing.from_string s in
      interpret lexbuf);
  [%expect {|
    :1:4: Operands to '<' must both be numbers.
    :1:1: Operands to '>' must both be numbers.
    :1:1: Operands to '+' must both be numbers or strings.
    :1:1: Operands to '+' must both be numbers or strings.
    |}]
