open! Core

(* We really, only check that assignment is to valid lvalues... *)
let rec check_declaration (t : Ast.declaration) =
  match t with
  | Ast.Class_decl t -> check_class t
  | Ast.Func_decl t -> check_function t
  | Ast.Var_decl t -> check_var t
  | Ast.Stmt_decl t -> check_statement t

and check_class ({ body; _ } : Ast.class_decl) =
  List.iter body ~f:check_function

and check_function ({ body; _ } : Ast.func) =
  List.iter body ~f:check_declaration

and check_var ({ init; _ } : Ast.var_decl) =
  match init with None -> () | Some expr -> check_expr expr

and check_statement s =
  match s with
  | Ast.Expr_stmt e -> check_expr e
  | Ast.For_stmt { init; cond; step; body } ->
      (match init with
      | None -> ()
      | Decl { init; _ } -> (
          match init with None -> () | Some e -> check_expr e)
      | Expr e -> check_expr e);
      (match cond with None -> () | Some e -> check_expr e);
      (match step with None -> () | Some e -> check_expr e);
      check_statement body
  | Ast.If_stmt { cond; consequent; alternative } -> (
      check_expr cond;
      check_statement consequent;
      match alternative with None -> () | Some s -> check_statement s)
  | Ast.Print_stmt e -> check_expr e
  | Ast.Return_stmt e -> ( match e with None -> () | Some e -> check_expr e)
  | Ast.While_stmt { cond; body } ->
      check_expr cond;
      check_statement body
  | Ast.Block_stmt decls -> List.iter decls ~f:check_declaration

and check_expr e =
  match e with
  | Ast.Assign_expr ({ lhs; rhs }, (fname, lnum, cnum)) -> (
      check_expr rhs;
      (* ensure that lhs is an lvalue *)
      match lhs with
      | { primary; calls = [] } -> (
          match primary with
          | Ast.Var_expr _ -> ()
          | _ ->
              failwith
              @@ sprintf "%s:%d:%d: Expected lvalue before '='." fname lnum cnum
          )
      | { calls; _ } -> (
          match List.last_exn calls with
          | Member _ -> ()
          | _ ->
              failwith
              @@ sprintf "%s:%d:%d: Expected lvalue befor '='." fname lnum cnum)
      )
  | Ast.Or_expr (e1, e2) ->
      check_expr e1;
      check_expr e2
  | Ast.And_expr (e1, e2) ->
      check_expr e1;
      check_expr e2
  | Ast.Eq_expr (e1, e2) ->
      check_expr e1;
      check_expr e2
  | Ast.Neq_expr (e1, e2) ->
      check_expr e1;
      check_expr e2
  | Ast.Lt_expr (e1, e2) ->
      check_expr e1;
      check_expr e2
  | Ast.Leq_expr (e1, e2) ->
      check_expr e1;
      check_expr e2
  | Ast.Gt_expr (e1, e2) ->
      check_expr e1;
      check_expr e2
  | Ast.Geq_expr (e1, e2) ->
      check_expr e1;
      check_expr e2
  | Ast.Add_expr (e1, e2) ->
      check_expr e1;
      check_expr e2
  | Ast.Sub_expr (e1, e2) ->
      check_expr e1;
      check_expr e2
  | Ast.Mult_expr (e1, e2) ->
      check_expr e1;
      check_expr e2
  | Ast.Div_expr (e1, e2) ->
      check_expr e1;
      check_expr e2
  | Ast.Neg_expr e -> check_expr e
  | Ast.Minus_expr e -> check_expr e
  | Ast.Call_expr { primary; calls } ->
      check_atom_expr primary;
      List.iter calls ~f:check_call_t

and check_atom_expr e = match e with Ast.Expr_expr e -> check_expr e | _ -> ()

and check_call_t t =
  match t with Ast.Call xs -> List.iter xs ~f:check_expr | Ast.Member _ -> ()
