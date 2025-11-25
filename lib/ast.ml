open! Core

type position = Lexing.position

let sexp_of_position _ = Sexp.Atom "pos"

type assign_expr = {
  (* this should really be an LValue. *)
  lhs : call_expr;
  rhs : expr;
}

and call_t = Call of expr list | Member of string
and call_expr = { primary : atom_expr; calls : call_t list }

and atom_expr =
  | Bool_expr of bool
  | Nil_expr
  | This_expr
  | Number_expr of float
  | String_expr of string
  | Var_expr of string * position
  | Super_expr of string * position
  | Expr_expr of expr * position

and expr =
  | Assign_expr of assign_expr * position
  | Or_expr of expr * expr * position
  | And_expr of expr * expr * position
  | Eq_expr of expr * expr * position
  | Neq_expr of expr * expr * position
  | Lt_expr of expr * expr * position
  | Leq_expr of expr * expr * position
  | Gt_expr of expr * expr * position
  | Geq_expr of expr * expr * position
  | Add_expr of expr * expr * position
  | Sub_expr of expr * expr * position
  | Mult_expr of expr * expr * position
  | Div_expr of expr * expr * position
  | Neg_expr of expr * position
  | Minus_expr of expr * position
  | Call_expr of call_expr * position
[@@deriving sexp_of]

type class_decl = { name : string; parent : string option; body : func list }
and var_decl = { name : string; init : expr option }

and func = {
  name : string;
  params : string list;
  body : declaration list;
  pos : position;
}

and declaration =
  | Class_decl of class_decl
  | Func_decl of func
  | Var_decl of var_decl
  | Stmt_decl of statement

and for_init_t = Decl of var_decl | Expr of expr | None

and for_stmt = {
  init : for_init_t;
  cond : expr option;
  step : expr option;
  body : statement;
}

and if_stmt = {
  cond : expr;
  consequent : statement;
  alternative : statement option;
}

and while_stmt = { cond : expr; body : statement }

and statement =
  | Expr_stmt of expr
  | For_stmt of for_stmt
  | If_stmt of if_stmt
  | Print_stmt of expr
  | Return_stmt of expr option * position
  | While_stmt of while_stmt
  | Block_stmt of declaration list
[@@deriving sexp_of]

let print ~outx t =
  fprintf outx "%s\n%!" (Sexp.to_string @@ sexp_of_declaration t)
