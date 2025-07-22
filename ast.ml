(* ast.ml *)
type binop = Add | Sub | Mul | Div

type expr =
  | IntLit of int
  | Var    of string
  | Binop  of binop * expr * expr
