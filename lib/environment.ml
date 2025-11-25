open! Core

exception EnvError of Ast.position * string

type lox_object [@@deriving sexp_of]

type value =
  | Bool of bool
  | String of string
  | Number of float
  | Object of lox_object ref
  | Nil
[@@deriving sexp_of]

type t = (string, value) Hashtbl.t list

let rec find env ~name ~pos : value =
  match env with
  | [] -> raise (EnvError (pos, "Undefined variable '" ^ name ^ "'."))
  | hd :: tl -> (
      match Hashtbl.find hd name with None -> find tl ~name ~pos | Some v -> v)

let define ~name ~value = function
  | [] -> assert false
  | hd :: _ -> Hashtbl.update hd name ~f:(fun _ -> value)

let rec assign ~name ~(v : value) ~pos = function
  | [] -> raise (EnvError (pos, "Undefined variable '" ^ name ^ "'."))
  | hd :: tl -> (
      match Hashtbl.find hd name with
      | None -> assign ~name ~v ~pos tl
      | Some _ -> Hashtbl.update_and_return hd name ~f:(fun _ -> v))

let assign_at env ({ primary; calls } : Ast.call_expr) (v : value) =
  match calls with
  | [] -> (
      match primary with
      | Var_expr (name, pos) -> assign env ~name ~pos ~v
      (* semant should rule this case out *)
      | _ -> assert false)
  | _ -> assert false

let open_scope env = Hashtbl.create (module String) :: env
let empty () = open_scope []
