type t =
  | Left_brace
  | Right_brace
  | Left_bracket
  | Right_bracket
  | Colon
  | Comma
  | Str of string
  | Number of (int * string option)
  | Boolean of bool
  | Null
[@@deriving show]
