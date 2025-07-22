{
  (* lexer.mll *)
  open Parser   (* for token constructors *)
  exception LexError of string
}

rule read = parse
  | [' ' '\t' '\n']   { read lexbuf }
  | ['0'-'9']+ as d   { INT (int_of_string d) }
  | '+'               { PLUS }
  | '-'               { MINUS }
  | '*'               { TIMES }
  | '/'               { DIVIDE }
  | '('               { LPAREN }
  | ')'               { RPAREN }
  | ['a'-'z' 'A'-'Z']+ as id { ID id }
  | eof               { EOF }
  | _ as c            { raise (LexError (Printf.sprintf "unexpected %c" c)) }
