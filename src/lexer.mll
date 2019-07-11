{
let reservedWords = [
  (* Keywords *)
  ("else", Parser.ELSE);
  ("false", Parser.FALSE);
  ("if", Parser.IF);
  ("then", Parser.THEN);
  ("true", Parser.TRUE);
  ("in", Parser.IN);
  ("let", Parser.LET);
  ("and", Parser.ANDLET);
  ("fun", Parser.FUN);
  ("rec", Parser.REC);
]
}

rule main = parse
  (* ignore spacing and newline characters *)
  [' ' '\009' '\012' '\n']+     { main lexbuf }

| "-"? ['0'-'9']+
    { Parser.INTV (int_of_string (Lexing.lexeme lexbuf)) }

| "(" { Parser.LPAREN }
| ")" { Parser.RPAREN }
| ";;" { Parser.SEMISEMI }
| "+" { Parser.PLUS }
| "-" { Parser.MINUS }
| "*" { Parser.MULT }
| "/" { Parser.DIV }
| "<" { Parser.LT }
| "=" { Parser.EQ }
| "&&" { Parser.AND }
| "||" { Parser.OR }
| "->" { Parser.RARROW }

| "(*" { comment lexbuf; main lexbuf }

| ['a'-'z'] ['a'-'z' '0'-'9' '_' '\'']*
    { let id = Lexing.lexeme lexbuf in
      try
        List.assoc id reservedWords
      with
      _ -> Parser.ID id
     }
| eof { exit 0 }

and comment = parse
| "(*" { comment lexbuf; comment lexbuf }
| "*)" { () }
| _ { comment lexbuf }
| eof { () }

