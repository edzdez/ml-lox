open! Core

let interpret lexbuf =
  let ast = Lox.Parser.program Lox.Lexer.read lexbuf in
  Lox.Util.do_interpret ast

let%expect_test "works with basic arithmetic expressions" =
  let lexbuf =
    Lexing.from_string
      {|
        print 1 + 2;
        print "Hello, " + "world!";
        print 10 / 3;
        print 6 - 4;
        print 4 * 1;
    |}
  in
  interpret lexbuf;
  [%expect {|
    3
    Hello, world!
    3.3333333333333335
    2
    4
    |}]

let%expect_test "works with basic comparison expressions" =
  let lexbuf =
    Lexing.from_string
      {|
        print 1 < 2;
        print 2 < 2;
        print 2 <= 2;
        print 3 <= 2;
        print 1 > 2;
        print 2 > 2;
        print 2 >= 2;
        print 3 >= 2;
        print nil == nil;
        print 5 == 4;
        print "hi" == "hi";
        print "hi" == "bye";
        print 5 == true;
        print nil != nil;
        print 5 != 4;
        print "hi" != "hi";
        print "hi" != "bye";
        print 5 != true;
    |}
  in
  interpret lexbuf;
  [%expect
    {|
    true
    false
    true
    false
    false
    false
    true
    true
    true
    false
    true
    false
    false
    false
    true
    false
    true
    true
    |}]

let%expect_test "runtime error on type mismatches" =
  List.iter
    [
      {|print "hi" < 2;|};
      {|print 1 > true;|};
      {|print 4 + nil; |};
      {|print 5 + "hi";|};
    ] ~f:(fun s ->
      let lexbuf = Lexing.from_string s in
      interpret lexbuf);
  [%expect
    {|
    :1:10: Operands to '<' must both be numbers.
    :1:7: Operands to '>' must both be numbers.
    :1:7: Operands to '+' must both be numbers or strings.
    :1:7: Operands to '+' must both be numbers or strings.
    |}]
