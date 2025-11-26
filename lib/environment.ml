open! Core

exception EnvError of Ast.position * string

type 'b env = {
  locals : (string, 'b, String.comparator_witness) Map.t list;
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
  {
    locals = Map.of_alist_exn (module String) [ (name, ref value) ] :: locals;
    globals;
  }

let find_method (o : lox_object ref) ~name =
  let rec find_method = function
    | None -> None
    | Some c -> (
        match Hashtbl.find c.methods name with
        | None -> find_method c.parent
        | Some m -> Some m)
  in
  let open Option.Let_syntax in
  let%bind m = find_method (Some !o.base) in
  return @@ m o

let find_method_exn ~name ?(pos = Lexing.dummy_pos) (o : lox_object ref) =
  match find_method o ~name with
  | Some m -> m
  | None -> raise (EnvError (pos, sprintf "Undefined property '%s'." name))

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

let open_scope : (unit, value ref) t =
 fun { locals; globals } ->
  ((), { locals = Map.empty (module String) :: locals; globals })

let close_scope : (unit, value ref) t =
 fun { locals; globals } ->
  match locals with
  | [] -> assert false
  | _ :: tl -> ((), { locals = tl; globals })

let find_ref ~name : (value ref option, value ref) t =
 fun ({ locals; globals } as env) ->
  let rec aux locals =
    match locals with
    | [] -> (Hashtbl.find globals name, env)
    | hd :: tl -> (
        match Map.find hd name with None -> aux tl | v -> (v, env))
  in
  aux locals

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

let define ~name ~value ?(pos = Lexing.dummy_pos) : (unit, value ref) t =
 fun ({ locals; globals } as env) ->
  match locals with
  | [] ->
      Hashtbl.update globals name ~f:(fun _ -> ref value);
      ((), env)
  | hd :: tl ->
      let hd' =
        Map.update hd name ~f:(function
          | None -> ref value
          | Some _ ->
              raise
                (EnvError
                   (pos, sprintf "Redefinition of '%s' in this scope." name)))
      in
      ((), { locals = hd' :: tl; globals })

let assign ~name ~value ?(kind = "variable") ?(pos = Lexing.dummy_pos) :
    (value, value ref) t =
 fun ({ locals; globals } as env) ->
  let rec aux locals =
    match locals with
    | [] -> (
        match Hashtbl.find globals name with
        | None -> raise (EnvError (pos, sprintf "Undefined %s '%s'." kind name))
        | Some v ->
            v := value;
            (value, env))
    | hd :: tl -> (
        match Map.find hd name with
        | None -> aux tl
        | Some v ->
            v := value;
            (value, env))
  in
  aux locals

let class_env : (value ref env, value ref) t =
  let open Environment_monad.Let_syntax in
  let%bind old_env = get_env in
  let%bind () =
    set_env { locals = []; globals = Hashtbl.create (module String) }
  in
  let%bind class_env = get_env in
  let%bind () = set_env old_env in
  return class_env
