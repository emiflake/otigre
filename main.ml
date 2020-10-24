open Printf
exception Error of string

let () =
  let lexbuf = Lexing.from_channel stdin in
  try
    while true do
        let result = Parser.main Lexer.token lexbuf in
        printf "ok\n";
        printf "ident %s\n" result; print_newline(); flush stdout
    done
  with (Lexer.Error e) ->
    print_string e

(* let _ =
 *   let lexbuf = Lexing.from_channel stdin in
 *   while true do
 *     let result = Parser.main Lexer.token lexbuf in
 *     printf "ok\n";
 *     List.iter print_int result; print_newline(); flush stdout
 *   done *)
