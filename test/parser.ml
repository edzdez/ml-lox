open! Core

let parse_with_error lexbuf =
  try Lox.Parser.program Lox.Lexer.read lexbuf
  with Lox.Parser.Error ->
    printf "Parse error\n%!";
    []

let%expect_test "respects operator precedence" =
  In_channel.with_file "../test_programs/precedence.lox" ~f:(fun f ->
      let lexbuf = Lexing.from_channel f in
      List.iter ~f:(Lox.Ast.print ~outx:stdout) @@ parse_with_error lexbuf;
      [%expect
        {|
        (Stmt_decl(Print_stmt(Add_expr(Call_expr((primary(Number_expr 2))(calls())))(Mult_expr(Call_expr((primary(Number_expr 3))(calls())))(Call_expr((primary(Number_expr 4))(calls())))))))
        (Stmt_decl(Print_stmt(Sub_expr(Call_expr((primary(Number_expr 20))(calls())))(Mult_expr(Call_expr((primary(Number_expr 3))(calls())))(Call_expr((primary(Number_expr 4))(calls())))))))
        (Stmt_decl(Print_stmt(Add_expr(Call_expr((primary(Number_expr 2))(calls())))(Div_expr(Call_expr((primary(Number_expr 6))(calls())))(Call_expr((primary(Number_expr 3))(calls())))))))
        (Stmt_decl(Print_stmt(Sub_expr(Call_expr((primary(Number_expr 2))(calls())))(Div_expr(Call_expr((primary(Number_expr 6))(calls())))(Call_expr((primary(Number_expr 3))(calls())))))))
        (Stmt_decl(Print_stmt(Eq_expr(Call_expr((primary(Bool_expr false))(calls())))(Lt_expr(Call_expr((primary(Number_expr 2))(calls())))(Call_expr((primary(Number_expr 1))(calls())))))))
        (Stmt_decl(Print_stmt(Eq_expr(Call_expr((primary(Bool_expr false))(calls())))(Gt_expr(Call_expr((primary(Number_expr 1))(calls())))(Call_expr((primary(Number_expr 2))(calls())))))))
        (Stmt_decl(Print_stmt(Eq_expr(Call_expr((primary(Bool_expr false))(calls())))(Leq_expr(Call_expr((primary(Number_expr 2))(calls())))(Call_expr((primary(Number_expr 1))(calls())))))))
        (Stmt_decl(Print_stmt(Eq_expr(Call_expr((primary(Bool_expr false))(calls())))(Geq_expr(Call_expr((primary(Number_expr 1))(calls())))(Call_expr((primary(Number_expr 2))(calls())))))))
        (Stmt_decl(Print_stmt(Sub_expr(Call_expr((primary(Number_expr 1))(calls())))(Call_expr((primary(Number_expr 1))(calls()))))))
        (Stmt_decl(Print_stmt(Sub_expr(Call_expr((primary(Number_expr 1))(calls())))(Call_expr((primary(Number_expr 1))(calls()))))))
        (Stmt_decl(Print_stmt(Sub_expr(Call_expr((primary(Number_expr 1))(calls())))(Call_expr((primary(Number_expr 1))(calls()))))))
        (Stmt_decl(Print_stmt(Sub_expr(Call_expr((primary(Number_expr 1))(calls())))(Call_expr((primary(Number_expr 1))(calls()))))))
        (Stmt_decl(Print_stmt(Call_expr((primary(Expr_expr(Mult_expr(Call_expr((primary(Number_expr 2))(calls())))(Call_expr((primary(Expr_expr(Sub_expr(Call_expr((primary(Number_expr 6))(calls())))(Call_expr((primary(Expr_expr(Add_expr(Call_expr((primary(Number_expr 2))(calls())))(Call_expr((primary(Number_expr 2))(calls()))))))(calls()))))))(calls()))))))(calls())))))
        |}])

let%expect_test "assignment is right associative" =
  let lexbuf = Lexing.from_string "a = b = c;" in
  List.iter ~f:(Lox.Ast.print ~outx:stdout) @@ parse_with_error lexbuf;
  [%expect
    {| (Stmt_decl(Expr_stmt(Assign_expr((lhs((primary(Var_expr a))(calls())))(rhs(Assign_expr((lhs((primary(Var_expr b))(calls())))(rhs(Call_expr((primary(Var_expr c))(calls()))))))))))) |}]

let%expect_test "allows the empty class" =
  let lexbuf = Lexing.from_string "class Foo {}" in
  List.iter ~f:(Lox.Ast.print ~outx:stdout) @@ parse_with_error lexbuf;
  [%expect {| (Class_decl((name Foo)(parent())(body()))) |}]

let%expect_test "class inheritance" =
  let lexbuf = Lexing.from_string "class Foo < Bar {}" in
  List.iter ~f:(Lox.Ast.print ~outx:stdout) @@ parse_with_error lexbuf;
  [%expect {| (Class_decl((name Foo)(parent(Bar))(body()))) |}]

let%expect_test "for loops behave" =
  let lexbuf =
    Lexing.from_string
      {|
    for(;;) {}
    for(var i = 0; ;) {}
    for(i > 0; ;) {}
    for(; i < 0;) {}
    for(; ; i + 10) {}
    for(; ; i + 10) {}
    for(var i = 0; i < 10; i = i + 1) { print(i); }
    |}
  in
  List.iter ~f:(Lox.Ast.print ~outx:stdout) @@ parse_with_error lexbuf;
  [%expect
    {|
    (Stmt_decl(For_stmt((init None)(cond())(step())(body(Block_stmt())))))
    (Stmt_decl(For_stmt((init(Decl((name i)(init((Call_expr((primary(Number_expr 0))(calls()))))))))(cond())(step())(body(Block_stmt())))))
    (Stmt_decl(For_stmt((init(Expr(Gt_expr(Call_expr((primary(Var_expr i))(calls())))(Call_expr((primary(Number_expr 0))(calls()))))))(cond())(step())(body(Block_stmt())))))
    (Stmt_decl(For_stmt((init None)(cond((Lt_expr(Call_expr((primary(Var_expr i))(calls())))(Call_expr((primary(Number_expr 0))(calls()))))))(step())(body(Block_stmt())))))
    (Stmt_decl(For_stmt((init None)(cond())(step((Add_expr(Call_expr((primary(Var_expr i))(calls())))(Call_expr((primary(Number_expr 10))(calls()))))))(body(Block_stmt())))))
    (Stmt_decl(For_stmt((init None)(cond())(step((Add_expr(Call_expr((primary(Var_expr i))(calls())))(Call_expr((primary(Number_expr 10))(calls()))))))(body(Block_stmt())))))
    (Stmt_decl(For_stmt((init(Decl((name i)(init((Call_expr((primary(Number_expr 0))(calls()))))))))(cond((Lt_expr(Call_expr((primary(Var_expr i))(calls())))(Call_expr((primary(Number_expr 10))(calls()))))))(step((Assign_expr((lhs((primary(Var_expr i))(calls())))(rhs(Add_expr(Call_expr((primary(Var_expr i))(calls())))(Call_expr((primary(Number_expr 1))(calls())))))))))(body(Block_stmt((Stmt_decl(Print_stmt(Call_expr((primary(Expr_expr(Call_expr((primary(Var_expr i))(calls())))))(calls())))))))))))
    |}]

let%expect_test "funky calls" =
  let lexbuf =
    Lexing.from_string
      {|
      a;
      b.c();
      d().e.f();
      d.e.f.g().h.i.j;
      h.i(j,k,l).m.n(z,o(),p);
    |}
  in
  List.iter ~f:(Lox.Ast.print ~outx:stdout) @@ parse_with_error lexbuf;
  [%expect
    {|
    (Stmt_decl(Expr_stmt(Call_expr((primary(Var_expr a))(calls())))))
    (Stmt_decl(Expr_stmt(Call_expr((primary(Var_expr b))(calls((Member c)(Call())))))))
    (Stmt_decl(Expr_stmt(Call_expr((primary(Var_expr d))(calls((Call())(Member e)(Member f)(Call())))))))
    (Stmt_decl(Expr_stmt(Call_expr((primary(Var_expr d))(calls((Member e)(Member f)(Member g)(Call())(Member h)(Member i)(Member j)))))))
    (Stmt_decl(Expr_stmt(Call_expr((primary(Var_expr h))(calls((Member i)(Call((Call_expr((primary(Var_expr j))(calls())))(Call_expr((primary(Var_expr k))(calls())))(Call_expr((primary(Var_expr l))(calls())))))(Member m)(Member n)(Call((Call_expr((primary(Var_expr z))(calls())))(Call_expr((primary(Var_expr o))(calls((Call())))))(Call_expr((primary(Var_expr p))(calls())))))))))))
    |}]

let%expect_test "dangling else" =
  In_channel.with_file "../test_programs/if/dangling_else.lox" ~f:(fun f ->
      let lexbuf = Lexing.from_channel f in
      List.iter ~f:(Lox.Ast.print ~outx:stdout) @@ parse_with_error lexbuf);
  [%expect {|
    (Stmt_decl(If_stmt((cond(Call_expr((primary(Bool_expr true))(calls()))))(consequent(If_stmt((cond(Call_expr((primary(Bool_expr false))(calls()))))(consequent(Print_stmt(Call_expr((primary(String_expr bad))(calls())))))(alternative((Print_stmt(Call_expr((primary(String_expr good))(calls())))))))))(alternative()))))
    (Stmt_decl(If_stmt((cond(Call_expr((primary(Bool_expr false))(calls()))))(consequent(If_stmt((cond(Call_expr((primary(Bool_expr true))(calls()))))(consequent(Print_stmt(Call_expr((primary(String_expr bad))(calls())))))(alternative((Print_stmt(Call_expr((primary(String_expr bad))(calls())))))))))(alternative()))))
    |}]
