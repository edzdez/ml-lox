open! Core

let interpret lexbuf =
  let ast = Lox.Parser.program Lox.Lexer.read lexbuf in
  let env = Lox.Util.initial_env in
  ignore @@ Lox.Util.do_interpret ~env ast

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

let%expect_test "logical operators" =
  let lexbuf =
    Lexing.from_string
      {|
      print "a" or false;
      print nil or "a";
      print false and "a";
      print "a" and nil;
      |}
  in
  interpret lexbuf;
  [%expect {|
    a
    a
    false
    nil
    |}]

let%expect_test "logical operators should short circuit" =
  let lexbuf =
    Lexing.from_string
      {|
      var a = 10;
      true or (a = 5);
      print a;
      false and (a = 5);
      print a;
      |}
  in
  interpret lexbuf;
  [%expect {|
    10
    10
    |}]

let%expect_test "while loop" =
  let lexbuf =
    Lexing.from_string
      {|
      var i = 0;
      while (i < 5) i = i + 1;
      print i;
      |}
  in
  interpret lexbuf;
  [%expect {| 5 |}]

let%expect_test "while loop scoping" =
  let lexbuf =
    Lexing.from_string
      {|
      var i = 0;
      while (i < 5) {
          var j = 10;
          i = i + 1;
      }
      print j;
      |}
  in
  interpret lexbuf;
  [%expect {| :7:13: Undefined variable 'j'. |}]

let%expect_test "for loop" =
  let lexbuf =
    Lexing.from_string
      {|
      for (var i = 0; i < 5; i = i + 1) print i;
      |}
  in
  interpret lexbuf;
  [%expect {|
    0
    1
    2
    3
    4
    |}]

let%expect_test "for loop scoping" =
  let lexbuf =
    Lexing.from_string
      {|
      for (var i = 0; i < 5; i = i + 1) {
          var i = 10;
          print i;
      }
      print i;
      |}
  in
  interpret lexbuf;
  [%expect
    {|
    10
    10
    10
    10
    10
    :6:13: Undefined variable 'i'.
    |}]

let%expect_test "runtime error when calling a non-callable" =
  let lexbuf =
    Lexing.from_string {|
      "totally not a function"();
      |}
  in
  interpret lexbuf;
  [%expect {| :2:30: Can only call functions and classes. |}]

let%expect_test "can define and call functions" =
  let lexbuf =
    Lexing.from_string
      {|
      fun count(n) {
          if (n > 1) count (n - 1);
          print n;
      }
      count(3);
      |}
  in
  interpret lexbuf;
  [%expect {|
    1
    2
    3
    |}]

let%expect_test "can return within a function" =
  let lexbuf =
    Lexing.from_string
      {|
      fun fib(n) {
        if (n <= 1) return n;
        return fib(n - 2) + fib(n - 1);
      }

      for (var i = 0; i < 20; i = i + 1) {
        print fib(i);
      }
      |}
  in
  interpret lexbuf;
  [%expect
    {|
    0
    1
    1
    2
    3
    5
    8
    13
    21
    34
    55
    89
    144
    233
    377
    610
    987
    1597
    2584
    4181
    |}]

(* TODO: the new monadic thingamajig doesn't support this behavior
let%expect_test "reports a runtime error when returning outside of a function" =
  let lexbuf = Lexing.from_string {| return; |} in
  interpret lexbuf;
  [%expect {| :1:2: Unexpected return. |}]
*)

let%expect_test "functions are closures" =
  let lexbuf =
    Lexing.from_string
      {|
      fun makeCounter() {
        var i = 0;
        fun count() {
          i = i + 1;
          print i;
        }

        return count;
      }

      var counter = makeCounter();
      counter(); // "1".
      counter(); // "2".
      |}
  in
  interpret lexbuf;
  [%expect {|
    1
    2
    |}]
