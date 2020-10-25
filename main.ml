open Printf
exception Error of string

let fprint pp expr =
  printf "%s\n" (Format.asprintf "%a\n" pp expr)

let () =
  let file_chan = open_in "./example.tigre" in
  let lexbuf = Lexing.from_channel file_chan in
  try
      let result = Parser.main Lexer.token lexbuf in
      printf "Parse OK\n";
      fprint Ast.pp_expr result;
      print_newline();
      flush stdout
  with (Lexer.Error e) ->
    print_string e
