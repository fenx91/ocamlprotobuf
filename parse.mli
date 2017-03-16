type token =
  | IDENTIFIER of (string)
  | INT of (int)
  | STRING of (string)
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | MOD
  | TRUE
  | FALSE
  | EQ_TOK
  | L_TOK
  | LE_TOK
  | B_TOK
  | BE_TOK
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
  | PPRINT
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | LBRACK
  | RBRACK
  | QUOTE
  | DECLAREINT
  | DECLAREPROTO
  | DECLAREBOOL
  | DECLARESTR
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
