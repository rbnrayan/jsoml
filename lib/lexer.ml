type t = string

let make src = src
let from_in_channel chan = make (In_channel.input_all chan)

let rec tokenize l =
  match get_next_token l with Some tok, l -> tok :: tokenize l | None, _ -> []

and get_next_token l =
  if String.length l = 0 then (None, l)
  else
    let ch = String.get l 0 in
    let sub = String.sub l 1 (String.length l - 1) in
    let open Token in
    match ch with
    | '{' -> (Some Left_brace, sub)
    | '}' -> (Some Right_brace, sub)
    | '[' -> (Some Left_bracket, sub)
    | ']' -> (Some Right_bracket, sub)
    | ':' -> (Some Colon, sub)
    | ',' -> (Some Comma, sub)
    | '"' -> get_next_str_token sub
    | ch when is_numeric ch -> get_next_number_token l
    | ch when is_whitespace ch -> get_next_token sub
    | _ -> (
        match get_next_boolean_token l with
        | (Some _, _) as res -> res
        | None, _ -> failwith "Unknown token")

and get_next_str_token l =
  if String.index_opt l '"' = None then failwith "Unmatched quote for Token.Str"
  else
    let str =
      String.to_seq l |> Seq.take_while (fun ch -> ch != '"') |> String.of_seq
    in
    let length = String.length l in
    let str_length = String.length str in
    let sub = String.sub l (str_length + 1) (length - (str_length + 1)) in
    (Some (Token.Str str), sub)

and get_next_number_token l =
  let number =
    String.to_seq l |> Seq.take_while (fun ch -> is_numeric ch) |> String.of_seq
  in
  let length = String.length l in
  let number_length = String.length number in
  let sub = String.sub l number_length (length - number_length) in
  (Some (Token.Number (int_of_string number, None)), sub)

and get_next_boolean_token l =
  let is_whitespace_or_comma ch = is_whitespace ch || ch = ',' in
  let s =
    String.to_seq l
    |> Seq.take_while (fun ch -> not (is_whitespace_or_comma ch))
    |> String.of_seq
  in
  let sub =
    String.sub l (String.length s) (String.length l - String.length s)
  in
  if is_boolean s then (Some (Token.Boolean (string_to_bool s)), sub)
  else (None, l)

and is_whitespace = function ' ' | '\r' | '\n' | '\t' -> true | _ -> false

and is_numeric = function
  | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '0' -> true
  | _ -> false

and is_boolean = function "true" | "false" -> true | _ -> false

and string_to_bool = function
  | "true" -> true
  | "false" -> false
  | _ -> failwith "unexpected input, cannot convert it to bool"
