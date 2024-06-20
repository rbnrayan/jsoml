open Jsoml

let () =
  let json_ic = open_in "./bin/sample.json" in
  let lexer = Lexer.from_in_channel json_ic in
  Lexer.tokenize lexer |> List.map Token.show |> List.iter print_endline
