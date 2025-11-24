open! Core

let parse_with_error = Lox.Util.parse_with_error

let%expect_test "respects operator precedence" =
  In_channel.with_file "../test_programs/precedence.lox" ~f:(fun f ->
      let lexbuf = Lexing.from_channel f in
      List.iter ~f:(Lox.Ast.print ~outx:stdout) @@ parse_with_error lexbuf;
      [%expect
        {|
        (Stmt_decl(Print_stmt(Add_expr(Call_expr((primary(Number_expr 2))(calls()))pos)(Mult_expr(Call_expr((primary(Number_expr 3))(calls()))pos)(Call_expr((primary(Number_expr 4))(calls()))pos)pos)pos)))
        (Stmt_decl(Print_stmt(Sub_expr(Call_expr((primary(Number_expr 20))(calls()))pos)(Mult_expr(Call_expr((primary(Number_expr 3))(calls()))pos)(Call_expr((primary(Number_expr 4))(calls()))pos)pos)pos)))
        (Stmt_decl(Print_stmt(Add_expr(Call_expr((primary(Number_expr 2))(calls()))pos)(Div_expr(Call_expr((primary(Number_expr 6))(calls()))pos)(Call_expr((primary(Number_expr 3))(calls()))pos)pos)pos)))
        (Stmt_decl(Print_stmt(Sub_expr(Call_expr((primary(Number_expr 2))(calls()))pos)(Div_expr(Call_expr((primary(Number_expr 6))(calls()))pos)(Call_expr((primary(Number_expr 3))(calls()))pos)pos)pos)))
        (Stmt_decl(Print_stmt(Eq_expr(Call_expr((primary(Bool_expr false))(calls()))pos)(Lt_expr(Call_expr((primary(Number_expr 2))(calls()))pos)(Call_expr((primary(Number_expr 1))(calls()))pos)pos)pos)))
        (Stmt_decl(Print_stmt(Eq_expr(Call_expr((primary(Bool_expr false))(calls()))pos)(Gt_expr(Call_expr((primary(Number_expr 1))(calls()))pos)(Call_expr((primary(Number_expr 2))(calls()))pos)pos)pos)))
        (Stmt_decl(Print_stmt(Eq_expr(Call_expr((primary(Bool_expr false))(calls()))pos)(Leq_expr(Call_expr((primary(Number_expr 2))(calls()))pos)(Call_expr((primary(Number_expr 1))(calls()))pos)pos)pos)))
        (Stmt_decl(Print_stmt(Eq_expr(Call_expr((primary(Bool_expr false))(calls()))pos)(Geq_expr(Call_expr((primary(Number_expr 1))(calls()))pos)(Call_expr((primary(Number_expr 2))(calls()))pos)pos)pos)))
        (Stmt_decl(Print_stmt(Sub_expr(Call_expr((primary(Number_expr 1))(calls()))pos)(Call_expr((primary(Number_expr 1))(calls()))pos)pos)))
        (Stmt_decl(Print_stmt(Sub_expr(Call_expr((primary(Number_expr 1))(calls()))pos)(Call_expr((primary(Number_expr 1))(calls()))pos)pos)))
        (Stmt_decl(Print_stmt(Sub_expr(Call_expr((primary(Number_expr 1))(calls()))pos)(Call_expr((primary(Number_expr 1))(calls()))pos)pos)))
        (Stmt_decl(Print_stmt(Sub_expr(Call_expr((primary(Number_expr 1))(calls()))pos)(Call_expr((primary(Number_expr 1))(calls()))pos)pos)))
        (Stmt_decl(Print_stmt(Call_expr((primary(Expr_expr(Mult_expr(Call_expr((primary(Number_expr 2))(calls()))pos)(Call_expr((primary(Expr_expr(Sub_expr(Call_expr((primary(Number_expr 6))(calls()))pos)(Call_expr((primary(Expr_expr(Add_expr(Call_expr((primary(Number_expr 2))(calls()))pos)(Call_expr((primary(Number_expr 2))(calls()))pos)pos)pos))(calls()))pos)pos)pos))(calls()))pos)pos)pos))(calls()))pos)))
        |}])

let%expect_test "assignment is right associative" =
  let lexbuf = Lexing.from_string "a = b = c;" in
  List.iter ~f:(Lox.Ast.print ~outx:stdout) @@ parse_with_error lexbuf;
  [%expect
    {| (Stmt_decl(Expr_stmt(Assign_expr((lhs((primary(Var_expr a pos))(calls())))(rhs(Assign_expr((lhs((primary(Var_expr b pos))(calls())))(rhs(Call_expr((primary(Var_expr c pos))(calls()))pos)))pos)))pos))) |}]

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
    (Stmt_decl(For_stmt((init(Decl((name i)(init((Call_expr((primary(Number_expr 0))(calls()))pos))))))(cond())(step())(body(Block_stmt())))))
    (Stmt_decl(For_stmt((init(Expr(Gt_expr(Call_expr((primary(Var_expr i pos))(calls()))pos)(Call_expr((primary(Number_expr 0))(calls()))pos)pos)))(cond())(step())(body(Block_stmt())))))
    (Stmt_decl(For_stmt((init None)(cond((Lt_expr(Call_expr((primary(Var_expr i pos))(calls()))pos)(Call_expr((primary(Number_expr 0))(calls()))pos)pos)))(step())(body(Block_stmt())))))
    (Stmt_decl(For_stmt((init None)(cond())(step((Add_expr(Call_expr((primary(Var_expr i pos))(calls()))pos)(Call_expr((primary(Number_expr 10))(calls()))pos)pos)))(body(Block_stmt())))))
    (Stmt_decl(For_stmt((init None)(cond())(step((Add_expr(Call_expr((primary(Var_expr i pos))(calls()))pos)(Call_expr((primary(Number_expr 10))(calls()))pos)pos)))(body(Block_stmt())))))
    (Stmt_decl(For_stmt((init(Decl((name i)(init((Call_expr((primary(Number_expr 0))(calls()))pos))))))(cond((Lt_expr(Call_expr((primary(Var_expr i pos))(calls()))pos)(Call_expr((primary(Number_expr 10))(calls()))pos)pos)))(step((Assign_expr((lhs((primary(Var_expr i pos))(calls())))(rhs(Add_expr(Call_expr((primary(Var_expr i pos))(calls()))pos)(Call_expr((primary(Number_expr 1))(calls()))pos)pos)))pos)))(body(Block_stmt((Stmt_decl(Print_stmt(Call_expr((primary(Expr_expr(Call_expr((primary(Var_expr i pos))(calls()))pos)pos))(calls()))pos)))))))))
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
    (Stmt_decl(Expr_stmt(Call_expr((primary(Var_expr a pos))(calls()))pos)))
    (Stmt_decl(Expr_stmt(Call_expr((primary(Var_expr b pos))(calls((Member c)(Call()))))pos)))
    (Stmt_decl(Expr_stmt(Call_expr((primary(Var_expr d pos))(calls((Call())(Member e)(Member f)(Call()))))pos)))
    (Stmt_decl(Expr_stmt(Call_expr((primary(Var_expr d pos))(calls((Member e)(Member f)(Member g)(Call())(Member h)(Member i)(Member j))))pos)))
    (Stmt_decl(Expr_stmt(Call_expr((primary(Var_expr h pos))(calls((Member i)(Call((Call_expr((primary(Var_expr j pos))(calls()))pos)(Call_expr((primary(Var_expr k pos))(calls()))pos)(Call_expr((primary(Var_expr l pos))(calls()))pos)))(Member m)(Member n)(Call((Call_expr((primary(Var_expr z pos))(calls()))pos)(Call_expr((primary(Var_expr o pos))(calls((Call()))))pos)(Call_expr((primary(Var_expr p pos))(calls()))pos))))))pos)))
    |}]

let%expect_test "dangling else" =
  In_channel.with_file "../test_programs/if/dangling_else.lox" ~f:(fun f ->
      let lexbuf = Lexing.from_channel f in
      List.iter ~f:(Lox.Ast.print ~outx:stdout) @@ parse_with_error lexbuf);
  [%expect
    {|
    (Stmt_decl(If_stmt((cond(Call_expr((primary(Bool_expr true))(calls()))pos))(consequent(If_stmt((cond(Call_expr((primary(Bool_expr false))(calls()))pos))(consequent(Print_stmt(Call_expr((primary(String_expr bad))(calls()))pos)))(alternative((Print_stmt(Call_expr((primary(String_expr good))(calls()))pos)))))))(alternative()))))
    (Stmt_decl(If_stmt((cond(Call_expr((primary(Bool_expr false))(calls()))pos))(consequent(If_stmt((cond(Call_expr((primary(Bool_expr true))(calls()))pos))(consequent(Print_stmt(Call_expr((primary(String_expr bad))(calls()))pos)))(alternative((Print_stmt(Call_expr((primary(String_expr bad))(calls()))pos)))))))(alternative()))))
    |}]
