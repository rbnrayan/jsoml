type t

type ast =
  | ObjectJson of (string * ast) list
  | ArrayJson of ast list
  | NumberJson of float
  | StringJson of string
  | BooleanJson of bool
  | NullJson
[@@deriving show]

val make : Token.t list -> t
