open! Core
open Environment
open Environment.Environment_monad.Let_syntax

exception EvalError of Ast.position * string

type return = Return of value | Continue

let error ~pos msg = raise (EvalError (pos, msg))
let f_or_nil ~f = function None -> return Nil | Some v -> f v
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
  | Object o -> sprintf "%s instance" !o.base.name
  | Class { name; _ } -> name
  | Function { string_repr; _ } -> string_repr
  | Nil -> "nil"

let rec eval_atom_expr expr : (value, value ref) Environment.t =
  match expr with
  | Ast.Bool_expr b -> return @@ Bool b
  | Ast.Nil_expr -> return @@ Nil
  | Ast.This_expr pos -> (
      match%bind find ~name:"this" with
      | None -> error ~pos "Can't use 'this' outside of a class."
      | Some v -> return v)
  | Ast.Number_expr n -> return @@ Number n
  | Ast.String_expr s -> return @@ String s
  | Ast.Var_expr (name, pos) -> find_exn ~name ~kind:"variable" ~pos
  | Ast.Super_expr (name, pos) ->
      let%bind superclass =
        match%bind find ~name:"super" with
        | None -> error ~pos "Can't use 'super' in a class with no superclass."
        | Some (Class c) -> return c
        | _ -> assert false
      in
      let%bind this =
        match%bind find_exn ~name:"this" ~kind:"unreachable" with
        | Object o -> return o
        | _ -> assert false
      in
      let m = find_method_from_class_exn ~name ~pos (Some superclass) in
      return (Function (m this))
  | Ast.Expr_expr (e, _) -> eval_expr e

and eval_expr expr : (value, value ref) Environment.t =
  match expr with
  | Ast.Assign_expr ({ lhs = { primary; calls }; rhs }, pos) -> (
      let calls, property =
        List.fold_right calls ~init:([], None) ~f:(fun x -> function
          | _, None -> ([], Some x)
          | xs, last -> (x :: xs, last))
      in
      let%bind new_primary = eval_expr (Call_expr ({ primary; calls }, pos)) in
      let%bind value = eval_expr rhs in
      match (primary, new_primary, property) with
      | Var_expr (name, _), _, None -> assign ~name ~value ~kind:"variable" ~pos
      | _, Object o, Some (Member name) ->
          let obj_env = !o.env in
          let%bind curr_env = get_env in
          let%bind () = set_env obj_env in
          let%bind () = define ~name ~value ~pos in
          let%bind () = set_env curr_env in
          return value
      | _, _, Some (Member _) -> error ~pos "Only objects have properties."
      (* this case should be ruled out by semant *)
      | _ -> assert false)
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
      | _ -> error ~pos "Operands to '<' must both be numbers.")
  | Ast.Leq_expr (e1, e2, pos) -> (
      let%bind v1 = eval_expr e1 in
      let%bind v2 = eval_expr e2 in
      match (v1, v2) with
      | Number n1, Number n2 -> return @@ Bool Float.(n1 <= n2)
      | _ -> error ~pos "Operands to '<=' must both be numbers.")
  | Ast.Gt_expr (e1, e2, pos) -> (
      let%bind v1 = eval_expr e1 in
      let%bind v2 = eval_expr e2 in
      match (v1, v2) with
      | Number n1, Number n2 -> return @@ Bool Float.(n1 > n2)
      | _ -> error ~pos "Operands to '>' must both be numbers.")
  | Ast.Geq_expr (e1, e2, pos) -> (
      let%bind v1 = eval_expr e1 in
      let%bind v2 = eval_expr e2 in
      match (v1, v2) with
      | Number n1, Number n2 -> return @@ Bool Float.(n1 >= n2)
      | _ -> error ~pos "Operands to '>=' must both be numbers.")
  | Ast.Add_expr (e1, e2, pos) -> (
      let%bind v1 = eval_expr e1 in
      let%bind v2 = eval_expr e2 in
      match (v1, v2) with
      | Number n1, Number n2 -> return @@ Number (n1 +. n2)
      | String s1, String s2 -> return @@ String (s1 ^ s2)
      | _ -> error ~pos "Operands to '+' must both be numbers or strings.")
  | Ast.Sub_expr (e1, e2, pos) -> (
      let%bind v1 = eval_expr e1 in
      let%bind v2 = eval_expr e2 in
      match (v1, v2) with
      | Number n1, Number n2 -> return @@ Number (n1 -. n2)
      | _ -> error ~pos "Operands to '-' must both be numbers.")
  | Ast.Mult_expr (e1, e2, pos) -> (
      let%bind v1 = eval_expr e1 in
      let%bind v2 = eval_expr e2 in
      match (v1, v2) with
      | Number n1, Number n2 -> return @@ Number (n1 *. n2)
      | _ -> error ~pos "Operands to '*' must both be numbers.")
  | Ast.Div_expr (e1, e2, pos) -> (
      let%bind v1 = eval_expr e1 in
      let%bind v2 = eval_expr e2 in
      match (v1, v2) with
      | Number n1, Number n2 -> return @@ Number (n1 /. n2)
      | _ -> error ~pos "Operands to '/' must both be numbers.")
  | Ast.Neg_expr (e, _) ->
      let%bind v = eval_expr e in
      return @@ Bool (not (is_truthy v))
  | Ast.Minus_expr (e, pos) -> (
      let%bind v = eval_expr e in
      match v with
      | Number n -> return @@ Number Float.(-n)
      | _ -> error ~pos "Operand to '-' must be a number.")
  | Ast.Call_expr ({ primary; calls }, pos) ->
      let%bind callee = eval_atom_expr primary in
      foldM calls ~init:callee ~f:(fun callee -> function
        | Member name -> (
            match callee with
            | Object o ->
                let obj_env = !o.env in
                let%bind curr_env = get_env in
                let%bind () = set_env obj_env in
                let%bind prop =
                  match%bind find ~name with
                  | Some prop -> return prop
                  | None -> return (Function (find_method_exn o ~name ~pos))
                in
                let%bind () = set_env curr_env in
                return prop
            | _ -> error ~pos "Only objects have properties.")
        | Call args -> call ~callee ~args ~pos)

and call ~callee ~args ~pos : (value, value ref) Environment.t =
  let%bind arg_vals = mapM args ~f:eval_expr in
  let num_args = List.length arg_vals in
  match callee with
  | Class ({ arity; _ } as base) ->
      if num_args <> arity then
        error ~pos @@ sprintf "Expected %d arguments but got %d." arity num_args
      else
        let%bind env = class_env in
        let instance = ref { base; env } in
        let%bind _ =
          f_or_nil ~f:(fun { call; _ } -> call arg_vals)
          @@ find_method instance ~name:"init"
        in
        return (Object instance)
  | Function { arity; call; _ } ->
      if num_args <> arity then
        error ~pos @@ sprintf "Expected %d arguments but got %d." arity num_args
      else call arg_vals
  | _ -> error ~pos "Can only call functions and classes."

and execute_statement ~can_return t : (return, value ref) Environment.t =
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
      | false -> error ~pos "Can't return from top-level code."
      | true ->
          let%bind v = f_or_nil ~f:eval_expr expr in
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

and execute_declaration ~can_return t : (return, value ref) Environment.t =
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

and execute_class_decl { name; parent; body } ~pos :
    (unit, value ref) Environment.t =
  let%bind parent =
    match parent with
    | None -> return None
    | Some parent -> (
        match%bind find_exn ~name:parent ~kind:"class" ~pos with
        | Class c -> return (Some c)
        | _ -> error ~pos "Superclass must be a class.")
  in
  let c = Nil in
  let%bind () = define ~name ~value:c ~pos in
  let%bind c_ref = find_ref_exn ~name in
  let%bind env = get_env in
  let super_env =
    match parent with
    | None -> env
    | Some parent -> bind env ~name:"super" ~value:(Class parent)
  in
  let arity = ref 0 in
  let methods =
    List.map body ~f:(fun ({ name; params; _ } as f) ->
        let is_init = String.(name = "init") in
        if is_init then arity := List.length params;
        ( name,
          fun o ->
            let this_env = bind super_env ~name:"this" ~value:(Object o) in
            create_function f ~is_init ~env:this_env ))
  in
  c_ref :=
    Class
      {
        name;
        arity = !arity;
        methods = Hashtbl.of_alist_exn (module String) methods;
        parent;
      };
  return ()

and execute_func_decl ({ name; pos; _ } as func) :
    (unit, value ref) Environment.t =
  let f = Nil in
  let%bind () = define ~name ~value:f ~pos in
  let%bind ref = find_ref_exn ~name in
  let%bind closure_env = get_env in
  ref := Function (create_function func ~env:closure_env);
  return ()

and create_function ?(is_init = false) ~env { name; params; body; pos } =
  {
    arity = List.length params;
    string_repr = sprintf "<fn %s>" name;
    call =
      (fun args ->
        let%bind old_env = get_env in
        let%bind () = set_env env in
        let%bind () = open_scope in
        let zipped = List.zip_exn params args in
        let%bind _ =
          mapM zipped ~f:(fun (name, value) -> define ~name ~value ~pos)
        in
        let%bind res =
          foldM body ~init:Continue ~f:(function
            | Return _ as v -> fun _ -> return v
            | Continue -> fun decl -> execute_declaration ~can_return:true decl)
        in
        let%bind () = close_scope in
        match (res, is_init) with
        | Continue, false ->
            let%bind () = set_env old_env in
            return Nil
        | _, true ->
            let%bind this = find_exn ~name:"this" ~kind:"unreachable" in
            let%bind () = set_env old_env in
            return this
        | Return v, _ ->
            let%bind () = set_env old_env in
            return v);
  }

and execute_var_decl ?(pos = Lexing.dummy_pos) { name; init } :
    (unit, value ref) Environment.t =
  let%bind value = f_or_nil ~f:eval_expr init in
  define ~name ~value ~pos

and execute_for_init init : (unit, value ref) Environment.t =
  match init with
  | None -> return ()
  | Decl decl -> execute_var_decl decl
  | Expr expr -> eval_expr expr >>| ignore

and execute_loop ~can_return ?(cond = None) ?(step = None) ~body :
    (return, value ref) Environment.t =
  let%bind cond_v =
    match cond with None -> return @@ Bool true | Some expr -> eval_expr expr
  in
  if not @@ is_truthy cond_v then return Continue
  else
    match%bind execute_statement ~can_return body with
    | Continue ->
        let%bind _ = f_or_nil ~f:eval_expr step in
        execute_loop ~can_return ~cond ~step ~body
    | x -> return x
