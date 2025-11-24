open! Core

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
  | Var_expr of string
  | Super_expr of string
  | Expr_expr of expr
[@@deriving sexp]

and expr =
  | Assign_expr of assign_expr
  | Or_expr of expr * expr
  | And_expr of expr * expr
  | Eq_expr of expr * expr
  | Neq_expr of expr * expr
  | Lt_expr of expr * expr
  | Leq_expr of expr * expr
  | Gt_expr of expr * expr
  | Geq_expr of expr * expr
  | Add_expr of expr * expr
  | Sub_expr of expr * expr
  | Mult_expr of expr * expr
  | Div_expr of expr * expr
  | Neg_expr of expr
  | Minus_expr of expr
  | Call_expr of call_expr
[@@deriving sexp]

type class_decl = { name : string; parent : string option; body : func list }
and var_decl = { name : string; init : expr option }
and func = { name : string; params : string list; body : declaration list }

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
  | Return_stmt of expr option
  | While_stmt of while_stmt
  | Block_stmt of declaration list
[@@deriving sexp]

let print t = print_s (sexp_of_declaration t)
