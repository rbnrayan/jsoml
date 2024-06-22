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
    (* Skip whitespaces *)
    | ch when is_whitespace ch -> get_next_token (String.trim sub)
    | '{' -> (Some Left_brace, sub)
    | '}' -> (Some Right_brace, sub)
    | '[' -> (Some Left_bracket, sub)
    | ']' -> (Some Right_bracket, sub)
    | ':' -> (Some Colon, sub)
    | ',' -> (Some Comma, sub)
    | _ -> get_next_literal_token l

(*
  Literals:
  - String : "literal"
  - Number : 1234
  - Boolean: true | false
  - Null   : null
*)
and get_next_literal_token l =
  let get_literal l p = String.to_seq l |> Seq.take_while p |> String.of_seq in
  let ch = String.get l 0 in
  let open Token in
  match ch with
  | '"' -> (
      let quote_idx = String.index_from_opt l 1 '"' in
      match quote_idx with
      | Some idx ->
          let str = String.sub l 1 (idx - 1) in
          let sub = String.sub l (idx + 1) (String.length l - idx - 1) in
          (Some (Str str), sub)
      | None -> failwith "Unmatched quote for Token.Str")
  | ch when is_numeric ch ->
      let number = get_literal l (fun ch -> is_numeric ch) in
      let sub =
        String.sub l (String.length number)
          (String.length l - String.length number)
      in
      (Some (Number (int_of_string number, None)), sub)
  | _ -> (
      let literal =
        get_literal l (fun ch -> not (ch = ',' || is_whitespace ch))
      in
      let sub =
        String.sub l (String.length literal)
          (String.length l - String.length literal)
      in
      match literal with
      | "null" -> (Some Null, sub)
      | "true" | "false" -> (Some (Boolean (bool_of_string literal)), sub)
      | _ -> failwith (Printf.sprintf "Unknown literal `%s`\n" literal))

and is_whitespace = function ' ' | '\r' | '\n' | '\t' -> true | _ -> false

and is_numeric = function
  | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '0' -> true
  | _ -> false
