type un_op =
  | Not
  | Neg

let string_of_un_op = function
  | Not -> "!"
  | Neg -> "-"

type bin_op =
  | Add
  | Sub
  | Mul
  | Div
  | Rem
  | Assign
  | Lss
  | Gtr
  | Eql
  | Neq
  | Leq
  | Geq
  | Pipe
  | And
  | Or

let string_of_bin_op = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Rem -> "%"
  | Assign -> "="
  | Lss -> "<"
  | Gtr -> ">"
  | Eql -> "=="
  | Neq -> "!="
  | Leq -> ">="
  | Geq -> "<="
  | Pipe -> "|>"
  | And -> "and"
  | Or -> "or"

type expr =
  | Prog of expr list * expr option
  | Ident of string
  | Number of float
  | String of string
  | Boolean of bool
  | Unary of un_op * expr
  | Binary of bin_op * expr * expr
  | Block of expr list * expr option
  | Call of expr * expr list
  | Func of string list * expr
  | List of expr list * expr option
  | Map of (expr * expr) list
  | Index of expr * expr
  | Match of expr * arm list
  | Nil

and pattern =
  | PatWildcard
  | PatLit of expr
  | PatVar of string
  | PatOr of pattern * pattern
  | PatList of pattern list
  | PatListRest of pattern list * string option
  | PatMap of (expr * pattern) list

and arm =
  { pat : pattern;
    guard : expr option;
    res : expr
  }
