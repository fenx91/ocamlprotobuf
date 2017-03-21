{
(* 
 * Lexer for PF. 
 *)
open Parse
} 

let blank = [' ' '\012' '\r' '\t' '\n']

rule initial = parse
  "/*"  { let _ = comment lexbuf in initial lexbuf }
| "//"  { endline lexbuf }
| blank { initial lexbuf }
| '+'           { PLUS }
| '-'           { MINUS }
| '*'           { TIMES }
| '%'           { MOD }
| '/'           { DIV }
| "true"        { TRUE }
| "false"       { FALSE }
| "=="          { EQ_TOK }
| "<"           { L_TOK }
| "<="          { LE_TOK }
| ">"           { B_TOK }
| ">="          { BE_TOK }
| "!"           { NOT }
| "&&"          { AND }
| "||"          { OR }
| ":="          { SET }
| ';'           { SEMICOLON }
| "if"          { IF }
| "then"        { THEN }
| "else"        { ELSE }
| "while"       { WHILE }
| "do"          { DO }
| "print"       { PRINT }
| "pprint"      { PPRINT }
| "Integer"	{ DECLAREINT }
| "Boolean"     { DECLAREBOOL }
| "String"      { DECLARESTR }
| "end"		{ END }
| "Protobuf"	{ DECLAREPROTO }
| "readfrom"    { READFROM }
| "writeto"     { WRITETO }
| "sizeof"      { SIZEOF }
| "copyfrom"    { COPYFROM }
| "."           { PERIOD }
| "\""		{ QUOTE }
| "$"           { DOLLAR }
| "?"           { QUESTION }

| '('           { LPAREN }
| ')'           { RPAREN } 
| '{'           { LBRACE }
| '}'           { RBRACE } 
| '['           { LBRACK }
| ']'           { RBRACK }

| ("0x")?'-'?['0'-'9']+ {
  let str = Lexing.lexeme lexbuf in 
  INT(str) }

| ['A'-'Z''a'-'z''_']['0'-'9''A'-'Z''a'-'z''_''/']* {
  let str = Lexing.lexeme lexbuf in 
  IDENTIFIER(str)
  } 

| eof     { EOF } 
| _     { 
  raise(Failure("invalid character")) ;
  (* this is not the kind of error handling you want in real life *)
          }

and comment = parse
      "*/"  { () }
|     '\n'  { comment lexbuf }
|     eof   { raise(Failure("unterminated /* comment\n" )); }
|     _     { comment lexbuf }
and endline = parse
        '\n'      { initial lexbuf}
| _               { endline lexbuf}
|       eof       { EOF }
