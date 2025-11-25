open! Core

exception EnvError of Ast.position * string

type 'b env = (string, 'b, String.comparator_witness) Map.t

module Environment_basic : sig
  type ('a, 'b) t = 'b env -> 'a * 'b env

  include Monad.Basic2 with type ('a, 'b) t := ('a, 'b) t
end = struct
  type ('a, 'b) t = 'b env -> 'a * 'b env

  let return x = fun env -> (x, env)

  let bind m ~f =
   fun env ->
    let x, env' = m env in
    (f x) env'

  let map =
    `Custom
      (fun m ~f ->
        fun env ->
         let x, env' = m env in
         (f x, env'))
end

include Environment_basic
module Environment_monad = Monad.Make2 (Environment_basic)

type lox_object

and lox_function = {
  arity : int;
  call : value list -> (value, value ref) t;
  string_repr : string;
}

and value =
  | Bool of bool
  | String of string
  | Number of float
  | Object of lox_object ref
  | Function of lox_function
  | Nil

let get_env : (value ref env, value ref) t = fun env -> (env, env)
let set_env env : (unit, value ref) t = fun _ -> ((), env)

let find_ref ~name ~pos : (value ref, value ref) t =
 fun env ->
  match Map.find env name with
  | None -> raise (EnvError (pos, "Undefined variable '" ^ name ^ "'."))
  | Some v -> (v, env)

let find ~name ~pos : (value, value ref) t =
  let open Environment_monad.Let_syntax in
  let%bind ref = find_ref ~name ~pos in
  return !ref

let define ~name ~value : (unit, value ref) t =
 fun env -> ((), Map.update env name ~f:(fun _ -> ref value))

let assign ~name ~value ~pos : (value, value ref) t =
 fun env ->
  ( value,
    match Map.find env name with
    | None -> raise (EnvError (pos, "Undefined variable '" ^ name ^ "'."))
    | Some v ->
        v := value;
        env )

let assign_at ~lvalue:({ primary; calls } : Ast.call_expr) ~value :
    (value, value ref) t =
  match calls with
  | [] -> (
      match primary with
      | Var_expr (name, pos) -> assign ~name ~pos ~value
      (* semant should rule this case out *)
      | _ -> assert false)
  | _ -> assert false

let foldM (xs : 'a list) ~(init : 'b) ~(f : 'b -> 'a -> ('b, value ref) t) :
    ('b, value ref) t =
  let open Environment_monad.Let_syntax in
  List.fold_left xs ~init:(return init) ~f:(fun acc x ->
      let%bind acc' = acc in
      f acc' x)

let mapM (xs : 'a list) ~(f : 'a -> ('b, value ref) t) : ('b list, value ref) t
    =
  let open Environment_monad.Let_syntax in
  List.fold_left xs ~init:(return []) ~f:(fun acc x ->
      let%bind x' = f x in
      let%bind acc' = acc in
      return @@ (x' :: acc'))
  >>| List.rev

let run (m : ('a, value ref) t) ~(env : value ref env) : value ref env =
  snd @@ m env
