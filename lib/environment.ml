open! Core

exception EnvError of Ast.position * string

type 'b env = {
  locals : (string, 'b, String.comparator_witness) Map.t;
  globals : (string, 'b) Hashtbl.t;
}

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

type lox_object = { base : lox_class; env : value ref env }

and lox_class = {
  name : string;
  arity : int;
  methods : (string, lox_object ref -> lox_function) Hashtbl.t;
  parent : lox_class option;
}

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
  | Class of lox_class
  | Function of lox_function
  | Nil

let bind { locals; globals } ~name ~value =
  { locals = Map.update locals name ~f:(fun _ -> ref value); globals }

let rec find_method_from_class ~name = function
  | None -> None
  | Some c -> (
      match Hashtbl.find c.methods name with
      | None -> find_method_from_class ~name c.parent
      | Some m -> Some m)

let find_method_from_class_exn ~name ?(pos = Lexing.dummy_pos) c =
  match find_method_from_class ~name c with
  | None -> raise (EnvError (pos, sprintf "Undefined property '%s'." name))
  | Some m -> m

let find_method (o : lox_object ref) ~name =
  let open Option.Let_syntax in
  let%bind m = find_method_from_class ~name (Some !o.base) in
  return @@ m o

let find_method_exn ~name ?(pos = Lexing.dummy_pos) (o : lox_object ref) =
  match find_method o ~name with
  | None -> raise (EnvError (pos, sprintf "Undefined property '%s'." name))
  | Some m -> m

let get_env : (value ref env, value ref) t = fun env -> (env, env)
let set_env env : (unit, value ref) t = fun _ -> ((), env)

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

let find_ref ~name : (value ref option, value ref) t =
 fun ({ locals; globals } as env) ->
  match Map.find locals name with
  | None -> (Hashtbl.find globals name, env)
  | v -> (v, env)

let find_ref_exn ~name ?(kind = "variable") ?(pos = Lexing.dummy_pos) :
    (value ref, value ref) t =
  let open Environment_monad.Let_syntax in
  match%bind find_ref ~name with
  | None -> raise (EnvError (pos, sprintf "Undefined %s '%s'." kind name))
  | Some v -> return v

let find ~name : (value option, value ref) t =
  let open Environment_monad in
  find_ref ~name >>| Option.map ~f:( ! )

let find_exn ~name ?(kind = "variable") ?(pos = Lexing.dummy_pos) :
    (value, value ref) t =
  let open Environment_monad.Let_syntax in
  let%bind ref = find_ref_exn ~name ~kind ~pos in
  return !ref

let define ?(global = false) ~name ~value : (unit, value ref) t =
 fun ({ locals; globals } as env) ->
  if global then (
    Hashtbl.update globals name ~f:(fun _ -> ref value);
    ((), env))
  else ((), { locals = Map.update locals name ~f:(fun _ -> ref value); globals })

let assign ~name ~value ?(kind = "variable") ?(pos = Lexing.dummy_pos) :
    (value, value ref) t =
 fun ({ locals; globals } as env) ->
  match Map.find locals name with
  | Some v ->
      v := value;
      (value, env)
  | None -> (
      match Hashtbl.find globals name with
      | None -> raise (EnvError (pos, sprintf "Undefined %s '%s'." kind name))
      | Some v ->
          v := value;
          (value, env))

let class_env : (value ref env, value ref) t =
  let open Environment_monad.Let_syntax in
  let%bind old_env = get_env in
  let%bind () =
    set_env
      {
        locals = Map.empty (module String);
        globals = Hashtbl.create (module String);
      }
  in
  let%bind class_env = get_env in
  let%bind () = set_env old_env in
  return class_env
