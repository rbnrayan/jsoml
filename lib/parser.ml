type t = Token.t list

type ast =
  | ObjectJson of (string * ast) list
  | ArrayJson of ast list
  | NumberJson of float
  | StringJson of string
  | BooleanJson of bool
  | NullJson
[@@deriving show]

let make tokens = tokens
