open! Core
open Environment

exception EvalError of Ast.position * string
exception Return of Ast.position * value

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
  | Object o -> "ref#" ^ Sexp.to_string (sexp_of_lox_object !o)
  | Function { string_repr; _ } -> string_repr
  | Nil -> "nil"

let rec eval_atom_expr env (expr : Ast.atom_expr) =
  match expr with
  | Ast.Bool_expr b -> Bool b
  | Ast.Nil_expr -> Nil
  | Ast.This_expr -> assert false
  | Ast.Number_expr n -> Number n
  | Ast.String_expr s -> String s
  | Ast.Var_expr (name, pos) -> find ~name ~pos env
  | Ast.Super_expr (_e, _) -> assert false
  | Ast.Expr_expr (e, _) -> eval_expr env e

and eval_expr env (expr : Ast.expr) =
  match expr with
  | Ast.Assign_expr ({ lhs; rhs }, _) ->
      let rhs = eval_expr env rhs in
      assign_at env lhs rhs
  | Ast.Or_expr (e1, e2, _) ->
      let v1 = eval_expr env e1 in
      if is_truthy v1 then v1 else eval_expr env e2
  | Ast.And_expr (e1, e2, _) ->
      let v1 = eval_expr env e1 in
      if not @@ is_truthy v1 then v1 else eval_expr env e2
  | Ast.Eq_expr (e1, e2, _) ->
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      Bool (are_equal v1 v2)
  | Ast.Neq_expr (e1, e2, _) ->
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      Bool (not @@ are_equal v1 v2)
  | Ast.Lt_expr (e1, e2, pos) -> (
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      match (v1, v2) with
      | Number n1, Number n2 -> Bool Float.(n1 < n2)
      | _ -> raise (EvalError (pos, "Operands to '<' must both be numbers.")))
  | Ast.Leq_expr (e1, e2, pos) -> (
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      match (v1, v2) with
      | Number n1, Number n2 -> Bool Float.(n1 <= n2)
      | _ -> raise (EvalError (pos, "Operands to '<=' must both be numbers.")))
  | Ast.Gt_expr (e1, e2, pos) -> (
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      match (v1, v2) with
      | Number n1, Number n2 -> Bool Float.(n1 > n2)
      | _ -> raise (EvalError (pos, "Operands to '>' must both be numbers.")))
  | Ast.Geq_expr (e1, e2, pos) -> (
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      match (v1, v2) with
      | Number n1, Number n2 -> Bool Float.(n1 >= n2)
      | _ -> raise (EvalError (pos, "Operands to '>=' must both be numbers.")))
  | Ast.Add_expr (e1, e2, pos) -> (
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      match (v1, v2) with
      | Number n1, Number n2 -> Number (n1 +. n2)
      | String s1, String s2 -> String (s1 ^ s2)
      | _ ->
          raise
            (EvalError (pos, "Operands to '+' must both be numbers or strings."))
      )
  | Ast.Sub_expr (e1, e2, pos) -> (
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      match (v1, v2) with
      | Number n1, Number n2 -> Number (n1 -. n2)
      | _ -> raise (EvalError (pos, "Operands to '-' must both be numbers.")))
  | Ast.Mult_expr (e1, e2, pos) -> (
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      match (v1, v2) with
      | Number n1, Number n2 -> Number (n1 *. n2)
      | _ -> raise (EvalError (pos, "Operands to '*' must both be numbers.")))
  | Ast.Div_expr (e1, e2, pos) -> (
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      match (v1, v2) with
      | Number n1, Number n2 -> Number (n1 /. n2)
      | _ -> raise (EvalError (pos, "Operands to '/' must both be numbers.")))
  | Ast.Neg_expr (e, _) ->
      let v = eval_expr env e in
      Bool (not (is_truthy v))
  | Ast.Minus_expr (e, pos) -> (
      let v = eval_expr env e in
      match v with
      | Number n -> Number Float.(-n)
      | _ -> raise (EvalError (pos, "Operand to '-' must be a number.")))
  | Ast.Call_expr ({ primary; calls }, pos) ->
      let callee = eval_atom_expr env primary in
      List.fold_left calls ~init:callee ~f:(fun _callee -> function
        | Member _ -> assert false
        | Call args -> call env ~callee ~args ~pos)

and call env ~callee ~args ~pos =
  let arg_vals = List.map args ~f:(eval_expr env) in
  let num_args = List.length arg_vals in
  match callee with
  | Object _ -> assert false
  | Function { arity; call; _ } -> (
      if num_args <> arity then
        raise
          (EvalError
             ( pos,
               "Expected " ^ Int.to_string arity ^ " arguments but got "
               ^ Int.to_string num_args ^ "." ))
      else try call env arg_vals with Return (_, v) -> v)
  | _ -> raise (EvalError (pos, "Can only call functions and classes."))

and execute_statement env (t : Ast.statement) : unit =
  match t with
  | Ast.Expr_stmt e -> ignore @@ eval_expr env e
  | Ast.For_stmt { init; cond; step; body } ->
      let env' = open_scope env in
      execute_for_init env' init;
      execute_loop ~cond ~step ~body env'
  | Ast.If_stmt { cond; consequent; alternative } -> (
      let v = eval_expr env cond in
      if is_truthy v then execute_statement env consequent
      else
        match alternative with
        | None -> ()
        | Some alternative -> execute_statement env alternative)
  | Ast.Print_stmt e ->
      let v = eval_expr env e in
      printf "%s\n%!" @@ stringify v
  | Ast.Return_stmt (expr, pos) ->
      let v = match expr with None -> Nil | Some expr -> eval_expr env expr in
      raise (Return (pos, v))
  | Ast.While_stmt { cond; body } -> execute_loop ~cond:(Some cond) ~body env
  | Ast.Block_stmt ss ->
      let env' = open_scope env in
      List.iter ss ~f:(execute_declaration env')

and execute_declaration env (t : Ast.declaration) =
  match t with
  | Ast.Class_decl _ -> assert false
  | Ast.Func_decl f -> execute_func_decl env f
  | Ast.Var_decl v -> execute_var_decl env v
  | Ast.Stmt_decl s -> execute_statement env s

and execute_func_decl env { name; params; body; _ } =
  define env ~name
    ~value:
      (Function
         {
           arity = List.length params;
           string_repr = "<fn " ^ name ^ ">";
           call =
             (fun env args ->
               let env = open_scope @@ [ List.last_exn env ] in
               let zipped = List.zip_exn params args in
               List.iter zipped ~f:(fun (name, value) ->
                   define env ~name ~value);
               List.iter body ~f:(execute_declaration env);
               Nil);
         })

and execute_var_decl env { name; init } =
  let v = match init with None -> Nil | Some e -> eval_expr env e in
  define env ~name ~value:v

and execute_for_init env init =
  match init with
  | None -> ()
  | Decl decl -> execute_var_decl env decl
  | Expr expr -> ignore @@ eval_expr env expr

and execute_loop ?(cond : Ast.expr option = None)
    ?(step : Ast.expr option = None) ~body env =
  let cond_v =
    match cond with None -> Bool true | Some expr -> eval_expr env expr
  in
  if is_truthy cond_v then (
    execute_statement env body;
    (match step with None -> () | Some expr -> ignore @@ eval_expr env expr);
    execute_loop ~cond ~step ~body env)
