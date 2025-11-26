open! Core
open Environment
open Environment.Environment_monad.Let_syntax

exception EvalError of Ast.position * string

type return = Return of value | Continue

let is_truthy v = match v with Bool b -> b | Nil -> false | _ -> true

let are_equal v1 v2 =
  match (v1, v2) with
  | Bool b1, Bool b2 -> Bool.(b1 = b2)
  | String s1, String s2 -> String.(s1 = s2)
  | Number n1, Number n2 -> Float.(n1 = n2)
  | Object o1, Object o2 -> phys_equal o1 o2
  | Function f1, Function f2 -> phys_equal f1 f2
  | Nil, Nil -> true
  | _ -> false

let stringify v =
  match v with
  | Bool b -> Bool.to_string b
  | String s -> s
  | Number n ->
      let s = Float.to_string n in
      if String.is_suffix s ~suffix:"." then String.drop_suffix s 1 else s
  | Object _ -> assert false
  | Class { name; _ } -> name
  | Function { string_repr; _ } -> string_repr
  | Nil -> "nil"

let rec eval_atom_expr (expr : Ast.atom_expr) : (value, value ref) Environment.t
    =
  match expr with
  | Ast.Bool_expr b -> return @@ Bool b
  | Ast.Nil_expr -> return @@ Nil
  | Ast.This_expr -> assert false
  | Ast.Number_expr n -> return @@ Number n
  | Ast.String_expr s -> return @@ String s
  | Ast.Var_expr (name, pos) -> find ~name ~pos
  | Ast.Super_expr (_e, _) -> assert false
  | Ast.Expr_expr (e, _) -> eval_expr e

and eval_expr (expr : Ast.expr) : (value, value ref) Environment.t =
  match expr with
  | Ast.Assign_expr ({ lhs; rhs }, _) ->
      let%bind rhs = eval_expr rhs in
      assign_at ~lvalue:lhs ~value:rhs
  | Ast.Or_expr (e1, e2, _) ->
      let%bind v1 = eval_expr e1 in
      if is_truthy v1 then return v1 else eval_expr e2
  | Ast.And_expr (e1, e2, _) ->
      let%bind v1 = eval_expr e1 in
      if not @@ is_truthy v1 then return v1 else eval_expr e2
  | Ast.Eq_expr (e1, e2, _) ->
      let%bind v1 = eval_expr e1 in
      let%bind v2 = eval_expr e2 in
      return @@ Bool (are_equal v1 v2)
  | Ast.Neq_expr (e1, e2, _) ->
      let%bind v1 = eval_expr e1 in
      let%bind v2 = eval_expr e2 in
      return @@ Bool (not @@ are_equal v1 v2)
  | Ast.Lt_expr (e1, e2, pos) -> (
      let%bind v1 = eval_expr e1 in
      let%bind v2 = eval_expr e2 in
      match (v1, v2) with
      | Number n1, Number n2 -> return @@ Bool Float.(n1 < n2)
      | _ -> raise (EvalError (pos, "Operands to '<' must both be numbers.")))
  | Ast.Leq_expr (e1, e2, pos) -> (
      let%bind v1 = eval_expr e1 in
      let%bind v2 = eval_expr e2 in
      match (v1, v2) with
      | Number n1, Number n2 -> return @@ Bool Float.(n1 <= n2)
      | _ -> raise (EvalError (pos, "Operands to '<=' must both be numbers.")))
  | Ast.Gt_expr (e1, e2, pos) -> (
      let%bind v1 = eval_expr e1 in
      let%bind v2 = eval_expr e2 in
      match (v1, v2) with
      | Number n1, Number n2 -> return @@ Bool Float.(n1 > n2)
      | _ -> raise (EvalError (pos, "Operands to '>' must both be numbers.")))
  | Ast.Geq_expr (e1, e2, pos) -> (
      let%bind v1 = eval_expr e1 in
      let%bind v2 = eval_expr e2 in
      match (v1, v2) with
      | Number n1, Number n2 -> return @@ Bool Float.(n1 >= n2)
      | _ -> raise (EvalError (pos, "Operands to '>=' must both be numbers.")))
  | Ast.Add_expr (e1, e2, pos) -> (
      let%bind v1 = eval_expr e1 in
      let%bind v2 = eval_expr e2 in
      match (v1, v2) with
      | Number n1, Number n2 -> return @@ Number (n1 +. n2)
      | String s1, String s2 -> return @@ String (s1 ^ s2)
      | _ ->
          raise
            (EvalError (pos, "Operands to '+' must both be numbers or strings."))
      )
  | Ast.Sub_expr (e1, e2, pos) -> (
      let%bind v1 = eval_expr e1 in
      let%bind v2 = eval_expr e2 in
      match (v1, v2) with
      | Number n1, Number n2 -> return @@ Number (n1 -. n2)
      | _ -> raise (EvalError (pos, "Operands to '-' must both be numbers.")))
  | Ast.Mult_expr (e1, e2, pos) -> (
      let%bind v1 = eval_expr e1 in
      let%bind v2 = eval_expr e2 in
      match (v1, v2) with
      | Number n1, Number n2 -> return @@ Number (n1 *. n2)
      | _ -> raise (EvalError (pos, "Operands to '*' must both be numbers.")))
  | Ast.Div_expr (e1, e2, pos) -> (
      let%bind v1 = eval_expr e1 in
      let%bind v2 = eval_expr e2 in
      match (v1, v2) with
      | Number n1, Number n2 -> return @@ Number (n1 /. n2)
      | _ -> raise (EvalError (pos, "Operands to '/' must both be numbers.")))
  | Ast.Neg_expr (e, _) ->
      let%bind v = eval_expr e in
      return @@ Bool (not (is_truthy v))
  | Ast.Minus_expr (e, pos) -> (
      let%bind v = eval_expr e in
      match v with
      | Number n -> return @@ Number Float.(-n)
      | _ -> raise (EvalError (pos, "Operand to '-' must be a number.")))
  | Ast.Call_expr ({ primary; calls }, pos) ->
      let%bind callee = eval_atom_expr primary in
      foldM calls ~init:callee ~f:(fun callee -> function
        | Member _ -> assert false
        | Call args -> call ~callee ~args ~pos)

and call ~(callee : value) ~(args : Ast.expr list) ~pos :
    (value, value ref) Environment.t =
  let%bind arg_vals = mapM args ~f:eval_expr in
  let num_args = List.length arg_vals in
  match callee with
  | Object _ -> assert false
  | Function { arity; call; _ } ->
      if num_args <> arity then
        raise
          (EvalError
             ( pos,
               "Expected " ^ Int.to_string arity ^ " arguments but got "
               ^ Int.to_string num_args ^ "." ))
      else call arg_vals
  | _ -> raise (EvalError (pos, "Can only call functions and classes."))

and execute_statement ~can_return (t : Ast.statement) :
    (return, value ref) Environment.t =
  match t with
  | Ast.Expr_stmt e ->
      let%bind _ = eval_expr e in
      return Continue
  | Ast.For_stmt { init; cond; step; body } -> (
      let%bind old_env = get_env in
      let%bind () = open_scope in
      let%bind () = execute_for_init init in
      match%bind execute_loop ~can_return ~cond ~step ~body with
      | Return _ as x -> return x
      | Continue ->
          let%bind () = close_scope in
          let%bind () = set_env old_env in
          return Continue)
  | Ast.If_stmt { cond; consequent; alternative } -> (
      let%bind v = eval_expr cond in
      if is_truthy v then execute_statement ~can_return consequent
      else
        match alternative with
        | None -> return Continue
        | Some alternative -> execute_statement ~can_return alternative)
  | Ast.Print_stmt e ->
      let%bind v = eval_expr e in
      printf "%s\n%!" @@ stringify v;
      return Continue
  | Ast.Return_stmt (expr, pos) -> (
      match can_return with
      | false -> raise (EvalError (pos, "Can't return from top-level code."))
      | true ->
          let%bind v =
            match expr with None -> return Nil | Some expr -> eval_expr expr
          in
          return (Return v))
  | Ast.While_stmt { cond; body } ->
      execute_loop ~can_return ~cond:(Some cond) ~step:None ~body
  | Ast.Block_stmt ss ->
      let%bind old_env = get_env in
      let%bind () = open_scope in
      let%bind res =
        foldM ss ~init:Continue ~f:(function
          | Return _ as v -> fun _ -> return v
          | Continue -> fun decl -> execute_declaration ~can_return decl)
      in
      let%bind () = close_scope in
      let%bind () = set_env old_env in
      return res

and execute_declaration ~can_return (t : Ast.declaration) :
    (return, value ref) Environment.t =
  match t with
  | Ast.Class_decl (c, pos) ->
      let%bind () = execute_class_decl c ~pos in
      return Continue
  | Ast.Func_decl f ->
      let%bind () = execute_func_decl f in
      return Continue
  | Ast.Var_decl (v, pos) ->
      let%bind () = execute_var_decl ~pos v in
      return Continue
  | Ast.Stmt_decl s -> execute_statement ~can_return s

and execute_class_decl { name; _ } ~pos : (unit, value ref) Environment.t =
  let c = Nil in
  let%bind () = define ~name ~value:c ~pos in
  let%bind ref = find_ref ~name ~pos:Lexing.dummy_pos in
  (* let%bind class_env = get_env in *)
  ref := Class { name };
  return ()

and execute_func_decl { name; params; body; pos } :
    (unit, value ref) Environment.t =
  let f = Nil in
  let%bind () = define ~name ~value:f ~pos in
  let%bind ref = find_ref ~name ~pos:Lexing.dummy_pos in
  let%bind closure_env = get_env in
  ref :=
    Function
      {
        arity = List.length params;
        string_repr = "<fn " ^ name ^ ">";
        call =
          (fun args ->
            let%bind old_env = get_env in
            let%bind () = set_env closure_env in
            let%bind () = open_scope in
            let zipped = List.zip_exn params args in
            let%bind _ =
              mapM zipped ~f:(fun (name, value) -> define ~name ~value ~pos)
            in
            let%bind res =
              foldM body ~init:Continue ~f:(function
                | Return _ as v -> fun _ -> return v
                | Continue ->
                    fun decl -> execute_declaration ~can_return:true decl)
            in
            let%bind () = close_scope in
            let%bind _ = set_env old_env in
            match res with Continue -> return Nil | Return v -> return v);
      };
  return ()

and execute_var_decl ~pos { name; init } : (unit, value ref) Environment.t =
  let%bind value =
    match init with None -> return Nil | Some e -> eval_expr e
  in
  define ~name ~value ~pos

and execute_for_init init : (unit, value ref) Environment.t =
  match init with
  | None -> return ()
  | Decl decl -> execute_var_decl ~pos:Lexing.dummy_pos decl
  | Expr expr -> eval_expr expr >>| ignore

and execute_loop ~can_return ?(cond : Ast.expr option = None)
    ?(step : Ast.expr option = None) ~body : (return, value ref) Environment.t =
  let%bind cond_v =
    match cond with None -> return @@ Bool true | Some expr -> eval_expr expr
  in
  match is_truthy cond_v with
  | false -> return Continue
  | true -> (
      match%bind execute_statement ~can_return body with
      | Continue ->
          let%bind _ =
            match step with None -> return Nil | Some expr -> eval_expr expr
          in
          execute_loop ~can_return ~cond ~step ~body
      | x -> return x)
