open! Core

exception SemantError of Ast.position * string

let error ~pos msg = raise (SemantError (pos, msg))

let rec check_declaration ?(is_init = false) ?(in_class = false)
    ?(has_parent = false) ?(curr_scope = None) (t : Ast.declaration) =
  match t with
  | Ast.Class_decl (t, pos) -> check_class ~pos t
  | Ast.Func_decl t -> check_function ~is_init ~in_class ~has_parent t
  | Ast.Var_decl (t, pos) -> check_var ~curr_scope ~in_class ~has_parent ~pos t
  | Ast.Stmt_decl t -> check_statement ~is_init ~in_class ~has_parent t

and check_class ~pos ({ name; parent; body } : Ast.class_decl) =
  (match parent with
  | None -> ()
  | Some parent ->
      if String.(name = parent) then
        error ~pos "A class can't inherit from itself.");
  List.iter body ~f:(fun ({ name; _ } as f) ->
      check_function
        ~is_init:String.(name = "init")
        ~in_class:true ~has_parent:(Option.is_some parent) f)

and check_function ~is_init ?(in_class = false) ?(has_parent = false)
    ({ body; params; pos; _ } : Ast.func) =
  if List.length params >= 255 then
    error ~pos "Can't have more than 255 parameters."
  else
    let scope =
      List.fold_right params
        ~init:(Hash_set.create (module String))
        ~f:(fun name acc ->
          if Hash_set.mem acc name then
            error ~pos @@ sprintf "Redefinition of '%s' in this scope." name
          else Hash_set.add acc name;
          acc)
    in
    let curr_scope = Some scope in
    List.iter body
      ~f:(check_declaration ~is_init ~in_class ~has_parent ~curr_scope)

and check_var ~curr_scope ?(in_class = false) ?(has_parent = false) ~pos
    ({ name; init } : Ast.var_decl) =
  (match curr_scope with
  | None -> ()
  | Some scope ->
      if Hash_set.mem scope name then
        error ~pos @@ sprintf "Redefinition of '%s' in this scope." name
      else Hash_set.add scope name);
  match init with
  | None -> ()
  | Some expr -> check_expr ~in_class ~has_parent expr

and check_statement ~is_init ?(in_class = false) ?(has_parent = false) s =
  match s with
  | Ast.Expr_stmt e -> check_expr ~in_class ~has_parent e
  | Ast.For_stmt { init; cond; step; body } ->
      (match init with
      | None -> ()
      | Decl { init; _ } -> (
          match init with
          | None -> ()
          | Some e -> check_expr ~in_class ~has_parent e)
      | Expr e -> check_expr ~in_class ~has_parent e);
      (match cond with
      | None -> ()
      | Some e -> check_expr ~in_class ~has_parent e);
      (match step with
      | None -> ()
      | Some e -> check_expr ~in_class ~has_parent e);
      check_statement ~is_init ~in_class ~has_parent body
  | Ast.If_stmt { cond; consequent; alternative } -> (
      check_expr ~in_class ~has_parent cond;
      check_statement ~is_init ~in_class ~has_parent consequent;
      match alternative with
      | None -> ()
      | Some s -> check_statement ~is_init ~in_class ~has_parent s)
  | Ast.Print_stmt e -> check_expr ~in_class ~has_parent e
  | Ast.Return_stmt (e, pos) -> (
      match e with
      | None -> ()
      | Some e ->
          if is_init then
            error ~pos "Can't use a nonempty return from an initializer"
          else check_expr ~in_class ~has_parent e)
  | Ast.While_stmt { cond; body } ->
      check_expr ~in_class ~has_parent cond;
      check_statement ~is_init ~in_class ~has_parent body
  | Ast.Block_stmt decls ->
      let curr_scope = Some (Hash_set.create (module String)) in
      List.iter decls ~f:(check_declaration ~curr_scope ~in_class ~has_parent)

and check_expr ?(in_class = false) ?(has_parent = false) e =
  match e with
  | Ast.Assign_expr ({ lhs; rhs }, pos) -> (
      check_expr ~in_class ~has_parent rhs;
      match lhs with
      | { primary; calls = [] } -> (
          match primary with
          | Ast.Var_expr _ -> ()
          | _ -> error ~pos "Expected lvalue before '='.")
      | { calls; _ } -> (
          match List.last_exn calls with
          | Member _ -> ()
          | _ -> error ~pos "Expected lvalue before '='."))
  | Ast.Or_expr (e1, e2, _) ->
      check_expr ~in_class ~has_parent e1;
      check_expr ~in_class ~has_parent e2
  | Ast.And_expr (e1, e2, _) ->
      check_expr ~in_class ~has_parent e1;
      check_expr ~in_class ~has_parent e2
  | Ast.Eq_expr (e1, e2, _) ->
      check_expr ~in_class ~has_parent e1;
      check_expr ~in_class ~has_parent e2
  | Ast.Neq_expr (e1, e2, _) ->
      check_expr ~in_class ~has_parent e1;
      check_expr ~in_class ~has_parent e2
  | Ast.Lt_expr (e1, e2, _) ->
      check_expr ~in_class ~has_parent e1;
      check_expr ~in_class ~has_parent e2
  | Ast.Leq_expr (e1, e2, _) ->
      check_expr ~in_class ~has_parent e1;
      check_expr ~in_class ~has_parent e2
  | Ast.Gt_expr (e1, e2, _) ->
      check_expr ~in_class ~has_parent e1;
      check_expr ~in_class ~has_parent e2
  | Ast.Geq_expr (e1, e2, _) ->
      check_expr ~in_class ~has_parent e1;
      check_expr ~in_class ~has_parent e2
  | Ast.Add_expr (e1, e2, _) ->
      check_expr ~in_class ~has_parent e1;
      check_expr ~in_class ~has_parent e2
  | Ast.Sub_expr (e1, e2, _) ->
      check_expr ~in_class ~has_parent e1;
      check_expr ~in_class ~has_parent e2
  | Ast.Mult_expr (e1, e2, _) ->
      check_expr ~in_class ~has_parent e1;
      check_expr ~in_class ~has_parent e2
  | Ast.Div_expr (e1, e2, _) ->
      check_expr ~in_class ~has_parent e1;
      check_expr ~in_class ~has_parent e2
  | Ast.Neg_expr (e, _) -> check_expr ~in_class ~has_parent e
  | Ast.Minus_expr (e, _) -> check_expr ~in_class ~has_parent e
  | Ast.Call_expr ({ primary; calls }, pos) ->
      check_atom_expr ~in_class ~has_parent primary;
      List.iter calls ~f:(check_call_t ~in_class ~has_parent ~pos)

and check_atom_expr ?(in_class = false) ?(has_parent = false) e =
  match e with
  | Ast.Expr_expr (e, _) -> check_expr ~in_class ~has_parent e
  | This_expr pos ->
      if not in_class then error ~pos "Can't use 'this' outside of a class."
  | Super_expr (_, pos) ->
      if not in_class then error ~pos "Can't use 'super' outside of a class."
      else if not has_parent then
        error ~pos "Can't use 'super' in a class with no superclass."
  | _ -> ()

and check_call_t ?(in_class = false) ?(has_parent = false) ~pos t =
  match t with
  | Ast.Call args ->
      if List.length args >= 255 then
        error ~pos "Can't have more than 255 arguments."
      else List.iter args ~f:(check_expr ~in_class ~has_parent)
  | Ast.Member _ -> ()
