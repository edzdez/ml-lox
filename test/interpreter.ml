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

let%expect_test "works with global variables" =
  let lexbuf =
    Lexing.from_string {|
    var a = 5;
    var b = 10;
    print a + b;
    |}
  in
  interpret lexbuf;
  [%expect {| 15 |}]

let%expect_test "allows redefinition of global variables" =
  let lexbuf =
    Lexing.from_string
      {|
    var a = 5;
    var b = 10;
    var a = 15;
    print a + b;
    |}
  in
  interpret lexbuf;
  [%expect {| 25 |}]

let%expect_test "runtime error on use of undefined variable" =
  let lexbuf = Lexing.from_string {|
    print a;
    |} in
  interpret lexbuf;
  [%expect {| :2:11: Undefined variable 'a'. |}]

let%expect_test "assignment to global variables" =
  let lexbuf =
    Lexing.from_string {|
    var a;
    a = 10;
    print a;
    |}
  in
  interpret lexbuf;
  [%expect {| 10 |}]

let%expect_test "runtime error when assigning to undefined variable" =
  let lexbuf = Lexing.from_string {|
    a = 10;
    print a;
    |} in
  interpret lexbuf;
  [%expect {| :2:5: Undefined variable 'a'. |}]

let%expect_test "block scoping" =
  let lexbuf =
    Lexing.from_string
      {|
      var a = "global a";
      var b = "global b";
      var c = "global c";
      {
        var a = "outer a";
        var b = "outer b";
        {
          var a = "inner a";
          print a;
          print b;
          print c;
        }
        print a;
        print b;
        print c;
      }
      print a;
      print b;
      print c;
      |}
  in
  interpret lexbuf;
  [%expect
    {|
    inner a
    outer b
    global c
    outer a
    outer b
    global c
    global a
    global b
    global c
    |}]

let%expect_test "if then else" =
  let lexbuf =
    Lexing.from_string
      {|
      if (true) if (false) print "no!"; else print "yes!";
      print "all done!";
      |}
  in
  interpret lexbuf;
  [%expect {|
    yes!
    all done!
    |}]
