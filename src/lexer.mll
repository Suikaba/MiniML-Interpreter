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
  ("match", Parser.MATCH);
  ("with", Parser.WITH);
  ("type", Parser.TYPE);
  ("of", Parser.OF);
]
}

rule main = parse
  (* ignore spacing and newline characters *)
  [' ' '\009' '\012' '\n']+     { main lexbuf }

| "-"? ['0'-'9']+
    { Parser.INTV (int_of_string (Lexing.lexeme lexbuf)) }

| "(" { Parser.LPAREN }
| ")" { Parser.RPAREN }
| ";" { Parser.SEMI }
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
| "!" { Parser.EXCLA }
| ":=" { Parser.COLONEQ }
| "," { Parser.COMMA }
| "[" { Parser.LBOXBRA }
| "]" { Parser.RBOXBRA }
| "|" { Parser.BAR }
| "::" { Parser.COLOCOLO }
| "@" { Parser.APPEND }
| "_" { Parser.PLACEHOLDER }

| "(*" { comment lexbuf; main lexbuf }

| ['a'-'z'] ['A'-'Z' 'a'-'z' '0'-'9' '_' '\'']*
    { let id = Lexing.lexeme lexbuf in
      try
        List.assoc id reservedWords
      with
      _ -> Parser.ID id
     }
| ['A'-'Z'] ['A'-'Z' 'a'-'z' '0'-'9' '_' '\'']*
    { let id = Lexing.lexeme lexbuf in
      Parser.UPPERCASE (Syntax.UpperId id)
    }
| eof { exit 0 }

and comment = parse
| "(*" { comment lexbuf; comment lexbuf }
| "*)" { () }
| _ { comment lexbuf }
| eof { () }

