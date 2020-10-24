{
  open Parser

  exception Error of string

  let comment_depth = ref 0
}


rule token = parse
  | [' ' '\t' '\n']
      { token lexbuf }
  | "/*"
      { comment_depth := 1;
        comment lexbuf;
        token lexbuf }

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
