type token =
  | IDENTIFIER of (string)
  | INT of (int)
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | MOD
  | TRUE
  | FALSE
  | EQ_TOK
  | LE_TOK
  | NOT
  | AND
  | OR
  | SKIP
  | SET
  | SEMICOLON
  | IF
  | THEN
  | ELSE
  | WHILE
  | DO
  | PRINT
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | LBRACK
  | RBRACK
  | QUOTE
  | DECLAREINT
  | DECLAREPROTO
  | END
  | PERIOD
  | DOLLAR
  | READFROM
  | WRITETO
  | COPYFROM
  | SIZEOF
  | EOF

val com :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Imp.com
