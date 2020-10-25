{
  open Parser

  exception Error of string

  let comment_depth = ref 0
}


rule token = parse
  | [' ' '\t' '\n']
      { token lexbuf }

  (* Start comment, goes into `comment` lexer *)
  | "/*"
      { comment_depth := 1;
        comment lexbuf;
        token lexbuf }

  (* Start string, goes into `read_string` lexer *)
  | '"' { read_string (Buffer.create 17) lexbuf }

  (* Punctuation *)
  | ',' { COMMA }
  | ':' { COLON }
  | ';' { SEMICOLON }

  | '(' { LPAREN }
  | ')' { RPAREN }
  | '[' { LBRACE }
  | ']' { RBRACE }
  | '{' { LBRACKET }
  | '}' { RBRACKET }

  | '.' { DOT }

  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { TIMES }
  | '/' { DIVIDE }

  | '=' { EQ }
  | "<>" { NEQ }
  | '<' { LT }
  | "<=" { LTE }
  | '>' { GT }
  | ">=" { GTE }

  | '&' { AND }
  | '|' { OR }
  | ":=" { ASSIGN }

  (* Keywords *)
  | "nil" { NIL }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "while" { WHILE }
  | "for" { FOR }
  | "let" { LET }
  | "in" { IN }
  | "end" { END }
  | "var" { VAR }
  | "type" { TYPE }
  | "break" { BREAK }
  | "do" { DO }
  | "function" { FUNCTION }
  | "array" { ARRAY }

  | eof
      { EOF }

  | ['0'-'9']+ as i
    { INT (int_of_string i) }
  | ['A'-'Z' 'a'-'z'] ['A'-'Z' 'a'-'z' '0'-'9' '_']* as id
    { ID id }

  | _
      { raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }

and comment = parse
    "/*"
      { incr comment_depth; comment lexbuf }
  | "*/"
      { decr comment_depth;
        if !comment_depth = 0 then () else comment lexbuf }
  | eof
      { raise (Error (Printf.sprintf "At offset %d: unterminated comment.\n" (Lexing.lexeme_start lexbuf))) }
  | _
      { comment lexbuf }

(* TODO: add support for other escape sequences *)
and read_string buf =
  parse
  | '"'       { STRING (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ { raise (Error (Printf.sprintf "At offset %d: Illegal string character: %s"
                        (Lexing.lexeme_start lexbuf)
                        (Lexing.lexeme lexbuf))) }
  | eof { raise (Error (Printf.sprintf "At offset %d: String is not terminated"
                           (Lexing.lexeme_start lexbuf))) }
