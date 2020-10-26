open Printf

exception Error of string

let pprint pp expr =
  printf "%s\n" (Format.asprintf "%a\n" pp expr)

let () =
  match Parse.file "./example.tigre" with
  | Result.Ok ast ->
    printf "Parse OK\n";
    pprint Ast.pp_expr ast;
    print_newline();
    flush stdout
  | Result.Error err_data ->
    match err_data with
    | (e, lexbuf) ->
      match e with
      | Lexer.Error e ->
        printf "Parse error %s\n" e
      | _ ->
        let pos = lexbuf.lex_curr_p in
        printf "Error at line %d\n" pos.pos_lnum
