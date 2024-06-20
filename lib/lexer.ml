type t = string

let init src = src
let from_in_channel chan = init (In_channel.input_all chan)

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
    | '"' ->
        let tok, l = get_next_str_token sub in
        (Some tok, l)
    | ch when is_numeric ch ->
        let tok, l = get_next_number_token l in
        (Some tok, l)
    | ch when is_whitespace ch -> get_next_token sub
    | _ -> failwith "Unknown token"

and get_next_str_token l =
  let str =
    String.to_seq l |> Seq.take_while (fun ch -> ch != '"') |> String.of_seq
  in
  let length = String.length l in
  let str_length = String.length str in
  let sub = String.sub l (str_length + 1) (length - (str_length + 1)) in
  (Token.Str str, sub)

and get_next_number_token l =
  let number =
    String.to_seq l |> Seq.take_while (fun ch -> is_numeric ch) |> String.of_seq
  in
  let length = String.length l in
  let number_length = String.length number in
  let sub = String.sub l number_length (length - number_length) in
  (Token.Number (int_of_string number), sub)

and is_whitespace = function ' ' | '\r' | '\n' | '\t' -> true | _ -> false

and is_numeric = function
  | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '0' -> true
  | _ -> false
