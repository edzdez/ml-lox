open! Core

exception EvalError of Ast.position * string

type lox_object [@@deriving sexp_of]

type value =
  | Bool of bool
  | String of string
  | Number of float
  | Object of lox_object ref
  | Nil
[@@deriving sexp_of]

type env = (string, lox_object, String.comparator_witness) Map.t

let is_truthy v = match v with Bool b -> b | Nil -> false | _ -> true

let are_equal v1 v2 =
  match (v1, v2) with
  | Bool b1, Bool b2 -> Bool.(b1 = b2)
  | String s1, String s2 -> String.(s1 = s2)
  | Number n1, Number n2 -> Float.(n1 = n2)
  | Object o1, Object o2 -> phys_equal o1 o2
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
  | Nil -> "nil"

let assign_at env ({ primary; calls } : Ast.call_expr) v =
  match calls with
  | [] -> (
      match primary with
      | Var_expr (name, pos) ->
          let env' =
            Map.update env name ~f:(function
              | None ->
                  raise (EvalError (pos, "Undefined variable '" ^ name ^ "'."))
              | Some _ -> v)
          in
          (v, env')
      (* semant should rule this case out *)
      | _ -> assert false)
  | _ -> assert false

let rec eval_atom_expr env (expr : Ast.atom_expr) =
  match expr with
  | Ast.Bool_expr b -> (Bool b, env)
  | Ast.Nil_expr -> (Nil, env)
  | Ast.This_expr -> assert false
  | Ast.Number_expr n -> (Number n, env)
  | Ast.String_expr s -> (String s, env)
  | Ast.Var_expr (name, pos) -> (
      match Map.find env name with
      | None -> raise (EvalError (pos, "Undefined variable '" ^ name ^ "'."))
      | Some v -> (v, env))
  | Ast.Super_expr (_e, _) -> assert false
  | Ast.Expr_expr (e, _) -> eval_expr env e

and eval_expr env (expr : Ast.expr) =
  match expr with
  | Ast.Assign_expr ({ lhs; rhs }, _) ->
      let rhs, env' = eval_expr env rhs in
      let rhs, env'' = assign_at env' lhs rhs in
      (rhs, env'')
  | Ast.Or_expr (_e1, _e2, _pos) -> assert false
  | Ast.And_expr (_e1, _e2, _pos) -> assert false
  | Ast.Eq_expr (e1, e2, _) ->
      let v1, env' = eval_expr env e1 in
      let v2, env'' = eval_expr env' e2 in
      (Bool (are_equal v1 v2), env'')
  | Ast.Neq_expr (e1, e2, _) ->
      let v1, env' = eval_expr env e1 in
      let v2, env'' = eval_expr env' e2 in
      (Bool (not @@ are_equal v1 v2), env'')
  | Ast.Lt_expr (e1, e2, pos) -> (
      let v1, env' = eval_expr env e1 in
      let v2, env'' = eval_expr env' e2 in
      match (v1, v2) with
      | Number n1, Number n2 -> (Bool Float.(n1 < n2), env'')
      | _ -> raise (EvalError (pos, "Operands to '<' must both be numbers.")))
  | Ast.Leq_expr (e1, e2, pos) -> (
      let v1, env' = eval_expr env e1 in
      let v2, env'' = eval_expr env' e2 in
      match (v1, v2) with
      | Number n1, Number n2 -> (Bool Float.(n1 <= n2), env'')
      | _ -> raise (EvalError (pos, "Operands to '<=' must both be numbers.")))
  | Ast.Gt_expr (e1, e2, pos) -> (
      let v1, env' = eval_expr env e1 in
      let v2, env'' = eval_expr env' e2 in
      match (v1, v2) with
      | Number n1, Number n2 -> (Bool Float.(n1 > n2), env'')
      | _ -> raise (EvalError (pos, "Operands to '>' must both be numbers.")))
  | Ast.Geq_expr (e1, e2, pos) -> (
      let v1, env' = eval_expr env e1 in
      let v2, env'' = eval_expr env' e2 in
      match (v1, v2) with
      | Number n1, Number n2 -> (Bool Float.(n1 >= n2), env'')
      | _ -> raise (EvalError (pos, "Operands to '>=' must both be numbers.")))
  | Ast.Add_expr (e1, e2, pos) -> (
      let v1, env' = eval_expr env e1 in
      let v2, env'' = eval_expr env' e2 in
      match (v1, v2) with
      | Number n1, Number n2 -> (Number (n1 +. n2), env'')
      | String s1, String s2 -> (String (s1 ^ s2), env'')
      | _ ->
          raise
            (EvalError (pos, "Operands to '+' must both be numbers or strings."))
      )
  | Ast.Sub_expr (e1, e2, pos) -> (
      let v1, env' = eval_expr env e1 in
      let v2, env'' = eval_expr env' e2 in
      match (v1, v2) with
      | Number n1, Number n2 -> (Number (n1 -. n2), env'')
      | _ -> raise (EvalError (pos, "Operands to '-' must both be numbers.")))
  | Ast.Mult_expr (e1, e2, pos) -> (
      let v1, env' = eval_expr env e1 in
      let v2, env'' = eval_expr env' e2 in
      match (v1, v2) with
      | Number n1, Number n2 -> (Number (n1 *. n2), env'')
      | _ -> raise (EvalError (pos, "Operands to '*' must both be numbers.")))
  | Ast.Div_expr (e1, e2, pos) -> (
      let v1, env' = eval_expr env e1 in
      let v2, env'' = eval_expr env' e2 in
      match (v1, v2) with
      | Number n1, Number n2 -> (Number (n1 /. n2), env'')
      | _ -> raise (EvalError (pos, "Operands to '/' must both be numbers.")))
  | Ast.Neg_expr (e, _) ->
      let v, env' = eval_expr env e in
      (Bool (not (is_truthy v)), env')
  | Ast.Minus_expr (e, pos) -> (
      let v, env' = eval_expr env e in
      match v with
      | Number n -> (Number Float.(-n), env')
      | _ -> raise (EvalError (pos, "Operand to '-' must be a number.")))
  | Ast.Call_expr ({ primary; calls }, _pos) -> (
      let callee, env' = eval_atom_expr env primary in
      match calls with [] -> (callee, env') | _ -> assert false)

(* NOTE: for now, we return values so that we may inspect them in the repl *)
and execute_statement env (t : Ast.statement) =
  match t with
  | Ast.Expr_stmt e -> eval_expr env e
  | Ast.For_stmt _ -> assert false
  | Ast.If_stmt _ -> assert false
  | Ast.Print_stmt e ->
      let v, env' = eval_expr env e in
      printf "%s\n%!" @@ stringify v;
      (v, env')
  | Ast.Return_stmt _ -> assert false
  | Ast.While_stmt _ -> assert false
  | Ast.Block_stmt _ -> assert false

and execute_declaration env (t : Ast.declaration) =
  match t with
  | Ast.Class_decl _ -> assert false
  | Ast.Func_decl _ -> assert false
  | Ast.Var_decl { name; init } ->
      let v, env' =
        match init with None -> (Nil, env) | Some e -> eval_expr env e
      in
      let env'' = Map.update env' name ~f:(fun _ -> v) in
      (v, env'')
  | Ast.Stmt_decl s -> execute_statement env s
