open Jsoml

let () =
  let json_ic = open_in "./bin/in.json" in
  let lexer = Lexer.from_in_channel json_ic in
  Lexer.tokenize lexer |> Parser.make |> Parser.parse |> Parser.ast_to_string
  |> print_endline
