open Printf
exception Error of string

let pprint pp expr =
  printf "%s\n" (Format.asprintf "%a\n" pp expr)

let () =
  let file_chan = open_in "./example.tigre" in
  let lexbuf = Lexing.from_channel file_chan ~with_positions:true in
  try
      let result = Parser.main Lexer.token lexbuf in
      printf "Parse OK\n";
      pprint Ast.pp_expr result;
      print_newline();
      flush stdout
  with
  | (Lexer.Error e) ->
    printf "Parse error %s\n" e
     | Parser.Error ->
       let pos = lexbuf.lex_curr_p in
       printf "Error at line %d\n" pos.pos_lnum
