type t =
  | Left_brace
  | Right_brace
  | Left_bracket
  | Right_bracket
  | Colon
  | Comma
  | Str of string
  | Number of int
  | Boolean of bool
[@@deriving show]
