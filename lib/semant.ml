open! Core

exception SemantError of Ast.position * string

(* We really, only check that assignment is to valid lvalues... *)
let rec check_declaration (t : Ast.declaration) =
  match t with
  | Ast.Class_decl t -> check_class t
  | Ast.Func_decl t -> check_function t
  | Ast.Var_decl (t, _) -> check_var t
  | Ast.Stmt_decl t -> check_statement t

and check_class ({ body; _ } : Ast.class_decl) =
  List.iter body ~f:check_function

and check_function ({ body; params; pos; _ } : Ast.func) =
  if List.length params >= 255 then
    raise (SemantError (pos, "Can't have more than 255 parameters."))
  else List.iter body ~f:check_declaration

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
  | Ast.Return_stmt (e, _) -> (
      match e with None -> () | Some e -> check_expr e)
  | Ast.While_stmt { cond; body } ->
      check_expr cond;
      check_statement body
  | Ast.Block_stmt decls -> List.iter decls ~f:check_declaration

and check_expr e =
  match e with
  | Ast.Assign_expr ({ lhs; rhs }, pos) -> (
      check_expr rhs;
      (* ensure that lhs is an lvalue *)
      match lhs with
      | { primary; calls = [] } -> (
          match primary with
          | Ast.Var_expr _ -> ()
          | _ -> raise (SemantError (pos, "Expected lvalue before '='.")))
      | { calls; _ } -> (
          match List.last_exn calls with
          | Member _ -> ()
          | _ -> raise (SemantError (pos, "Expected lvalue before '='."))))
  | Ast.Or_expr (e1, e2, _) ->
      check_expr e1;
      check_expr e2
  | Ast.And_expr (e1, e2, _) ->
      check_expr e1;
      check_expr e2
  | Ast.Eq_expr (e1, e2, _) ->
      check_expr e1;
      check_expr e2
  | Ast.Neq_expr (e1, e2, _) ->
      check_expr e1;
      check_expr e2
  | Ast.Lt_expr (e1, e2, _) ->
      check_expr e1;
      check_expr e2
  | Ast.Leq_expr (e1, e2, _) ->
      check_expr e1;
      check_expr e2
  | Ast.Gt_expr (e1, e2, _) ->
      check_expr e1;
      check_expr e2
  | Ast.Geq_expr (e1, e2, _) ->
      check_expr e1;
      check_expr e2
  | Ast.Add_expr (e1, e2, _) ->
      check_expr e1;
      check_expr e2
  | Ast.Sub_expr (e1, e2, _) ->
      check_expr e1;
      check_expr e2
  | Ast.Mult_expr (e1, e2, _) ->
      check_expr e1;
      check_expr e2
  | Ast.Div_expr (e1, e2, _) ->
      check_expr e1;
      check_expr e2
  | Ast.Neg_expr (e, _) -> check_expr e
  | Ast.Minus_expr (e, _) -> check_expr e
  | Ast.Call_expr ({ primary; calls }, loc) ->
      check_atom_expr primary;
      List.iter calls ~f:(check_call_t ~loc)

and check_atom_expr e =
  match e with Ast.Expr_expr (e, _) -> check_expr e | _ -> ()

and check_call_t ~loc t =
  match t with
  | Ast.Call args ->
      if List.length args >= 255 then
        raise (SemantError (loc, "Can't have more than 255 arguments."))
      else List.iter args ~f:check_expr
  | Ast.Member _ -> ()
