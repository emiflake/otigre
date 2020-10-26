
let lexbuf lb =
  try
    Result.Ok (Parser.main Lexer.token lb)
  with e ->
    Result.Error (e, lb)

let string str =
  Lexing.from_string str |> lexbuf

let file file_path =
  open_in file_path |> Lexing.from_channel |> lexbuf
