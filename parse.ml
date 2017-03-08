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

open Parsing;;
let _ = parse_error;;
# 2 "parse.mly"
(* 
 * Parser for our IMP concrete syntax. 
 * See http://caml.inria.fr/pub/docs/manual-ocaml/manual026.html
 * but basically it works just like Yacc/Bison.
 * See http://en.wikipedia.org/wiki/YACC
 *)

open Imp		    (* IMP abstract syntax *)

let error msg	= failwith msg

# 58 "parse.ml"
let yytransl_const = [|
  259 (* PLUS *);
  260 (* MINUS *);
  261 (* TIMES *);
  262 (* DIV *);
  263 (* MOD *);
  264 (* TRUE *);
  265 (* FALSE *);
  266 (* EQ_TOK *);
  267 (* LE_TOK *);
  268 (* NOT *);
  269 (* AND *);
  270 (* OR *);
  271 (* SKIP *);
  272 (* SET *);
  273 (* SEMICOLON *);
  274 (* IF *);
  275 (* THEN *);
  276 (* ELSE *);
  277 (* WHILE *);
  278 (* DO *);
  279 (* PRINT *);
  280 (* LPAREN *);
  281 (* RPAREN *);
  282 (* LBRACE *);
  283 (* RBRACE *);
  284 (* LBRACK *);
  285 (* RBRACK *);
  286 (* QUOTE *);
  287 (* DECLAREINT *);
  288 (* DECLAREPROTO *);
  289 (* END *);
  290 (* PERIOD *);
  291 (* DOLLAR *);
  292 (* READFROM *);
  293 (* WRITETO *);
  294 (* COPYFROM *);
  295 (* SIZEOF *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* IDENTIFIER *);
  258 (* INT *);
    0|]

let yylhs = "\255\255\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\003\000\003\000\003\000\003\000\003\000\
\005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\004\000\004\000\004\000\000\000"

let yylen = "\002\000\
\001\000\001\000\003\000\003\000\003\000\003\000\003\000\003\000\
\001\000\008\000\006\000\001\000\005\000\008\000\003\000\006\000\
\001\000\001\000\003\000\003\000\002\000\003\000\003\000\003\000\
\003\000\003\000\005\000\008\000\004\000\002\000\003\000\002\000\
\010\000\005\000\005\000\007\000\010\000\005\000\008\000\007\000\
\009\000\006\000\008\000\003\000\001\000\004\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\001\000\017\000\018\000\000\000\000\000\000\000\000\000\009\000\
\000\000\000\000\000\000\000\000\000\000\032\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\031\000\
\000\000\000\000\000\000\044\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\008\000\024\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\023\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\047\000\034\000\
\035\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\011\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\028\000\000\000\000\000\000\000\
\000\000\046\000\010\000\000\000\000\000\033\000"

let yydgoto = "\002\000\
\010\000\023\000\024\000\087\000\025\000"

let yysindex = "\037\000\
\085\255\000\000\053\255\063\255\063\255\035\255\085\255\255\254\
\030\255\025\255\101\255\035\255\062\255\093\255\108\255\115\255\
\000\000\000\000\000\000\063\255\063\255\144\255\149\255\000\000\
\128\255\122\255\035\255\158\255\246\254\000\000\145\255\085\255\
\173\255\158\255\250\254\141\255\176\255\177\255\178\255\000\255\
\073\255\043\255\179\255\035\255\035\255\035\255\035\255\035\255\
\035\255\035\255\063\255\063\255\085\255\085\255\124\255\000\000\
\180\255\025\255\115\255\000\000\035\255\050\255\181\255\154\255\
\155\255\159\255\151\255\000\000\000\000\153\255\166\255\166\255\
\075\255\158\255\158\255\158\255\158\255\175\255\000\000\242\254\
\025\255\156\255\158\255\162\255\163\255\011\255\000\000\000\000\
\000\000\191\255\193\255\194\255\182\255\195\255\183\255\035\255\
\035\255\090\255\196\255\169\255\172\255\249\254\168\255\085\255\
\171\255\035\255\158\255\158\255\184\255\185\255\187\255\000\000\
\202\255\000\000\203\255\205\255\251\254\186\255\158\255\201\255\
\035\255\189\255\190\255\045\255\000\000\206\255\035\255\158\255\
\000\000\000\000\000\000\192\255\158\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\174\255\000\000\000\000\000\000\000\000\000\000\
\000\000\209\000\000\000\000\000\000\000\000\000\000\000\133\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\002\000\000\000\000\000\000\000\000\000\
\000\000\016\000\059\255\000\000\000\000\000\000\000\000\126\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\030\000\025\000\000\000\000\000\000\000\000\000\000\000\
\000\000\001\000\000\000\000\000\000\000\000\000\183\000\208\000\
\158\000\226\000\244\000\096\255\100\255\121\255\000\000\000\000\
\035\000\000\000\033\000\000\000\000\000\069\255\000\000\000\000\
\000\000\000\000\000\000\000\000\058\000\000\000\131\255\066\000\
\000\000\000\000\000\000\000\000\034\000\188\255\000\000\000\000\
\000\000\000\000\091\000\099\000\000\000\000\000\188\255\067\000\
\000\000\000\000\000\000\000\000\000\000\000\000\124\000\132\255\
\132\000\000\000\000\000\188\255\000\000\000\000\000\000\137\000\
\100\000\000\000\000\000\000\000\157\000\000\000"

let yygindex = "\000\000\
\253\255\255\255\008\000\249\255\003\000"

let yytablesize = 533
let yytable = "\030\000\
\015\000\030\000\032\000\029\000\028\000\036\000\032\000\026\000\
\011\000\061\000\034\000\032\000\051\000\052\000\011\000\025\000\
\056\000\114\000\093\000\041\000\115\000\062\000\040\000\042\000\
\012\000\055\000\097\000\125\000\058\000\026\000\031\000\067\000\
\038\000\013\000\029\000\016\000\017\000\001\000\098\000\011\000\
\060\000\032\000\071\000\072\000\073\000\074\000\075\000\076\000\
\077\000\080\000\081\000\084\000\085\000\078\000\079\000\051\000\
\052\000\027\000\027\000\083\000\011\000\011\000\035\000\016\000\
\017\000\042\000\016\000\069\000\012\000\131\000\018\000\019\000\
\115\000\022\000\020\000\044\000\045\000\046\000\047\000\048\000\
\047\000\048\000\049\000\050\000\103\000\003\000\021\000\013\000\
\014\000\015\000\040\000\109\000\110\000\045\000\107\000\108\000\
\015\000\068\000\036\000\014\000\117\000\022\000\004\000\045\000\
\119\000\005\000\013\000\006\000\019\000\019\000\007\000\011\000\
\020\000\020\000\019\000\008\000\009\000\019\000\020\000\128\000\
\019\000\020\000\037\000\039\000\020\000\133\000\044\000\045\000\
\046\000\047\000\048\000\043\000\002\000\022\000\051\000\052\000\
\041\000\038\000\033\000\022\000\051\000\052\000\022\000\054\000\
\021\000\022\000\053\000\021\000\068\000\039\000\021\000\044\000\
\045\000\046\000\047\000\048\000\037\000\005\000\049\000\050\000\
\044\000\045\000\046\000\047\000\048\000\046\000\046\000\043\000\
\016\000\014\000\046\000\047\000\048\000\059\000\057\000\063\000\
\064\000\065\000\066\000\070\000\082\000\086\000\003\000\088\000\
\089\000\091\000\090\000\092\000\052\000\094\000\095\000\096\000\
\100\000\101\000\102\000\105\000\111\000\112\000\106\000\113\000\
\118\000\104\000\116\000\122\000\123\000\124\000\132\000\004\000\
\048\000\000\000\000\000\012\000\120\000\121\000\115\000\126\000\
\127\000\129\000\130\000\000\000\000\000\134\000\045\000\000\000\
\000\000\006\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\007\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\015\000\015\000\015\000\015\000\015\000\
\000\000\000\000\015\000\015\000\000\000\015\000\015\000\000\000\
\000\000\015\000\030\000\015\000\000\000\000\000\015\000\000\000\
\000\000\015\000\000\000\015\000\030\000\000\000\000\000\000\000\
\025\000\015\000\030\000\045\000\013\000\013\000\013\000\013\000\
\013\000\012\000\025\000\013\000\013\000\000\000\013\000\013\000\
\025\000\038\000\013\000\012\000\013\000\000\000\000\000\013\000\
\026\000\012\000\013\000\038\000\013\000\029\000\026\000\000\000\
\000\000\038\000\013\000\029\000\045\000\016\000\016\000\016\000\
\016\000\016\000\027\000\000\000\016\000\016\000\000\000\016\000\
\016\000\000\000\042\000\016\000\027\000\016\000\000\000\000\000\
\016\000\000\000\027\000\016\000\042\000\016\000\000\000\000\000\
\000\000\000\000\042\000\016\000\000\000\046\000\014\000\014\000\
\014\000\014\000\014\000\040\000\000\000\014\000\014\000\000\000\
\014\000\014\000\000\000\036\000\014\000\040\000\014\000\000\000\
\000\000\014\000\000\000\040\000\014\000\036\000\014\000\000\000\
\000\000\000\000\000\000\036\000\014\000\000\000\046\000\002\000\
\002\000\002\000\002\000\002\000\039\000\000\000\002\000\002\000\
\000\000\002\000\002\000\000\000\043\000\002\000\039\000\002\000\
\000\000\041\000\002\000\000\000\039\000\002\000\043\000\002\000\
\005\000\005\000\005\000\041\000\043\000\002\000\000\000\005\000\
\005\000\041\000\005\000\005\000\000\000\037\000\005\000\000\000\
\005\000\000\000\000\000\005\000\000\000\000\000\005\000\037\000\
\005\000\003\000\003\000\000\000\000\000\037\000\005\000\000\000\
\003\000\003\000\000\000\003\000\003\000\000\000\000\000\003\000\
\000\000\003\000\000\000\000\000\003\000\000\000\000\000\003\000\
\000\000\003\000\004\000\004\000\000\000\000\000\000\000\003\000\
\000\000\004\000\004\000\000\000\004\000\004\000\000\000\000\000\
\004\000\000\000\004\000\000\000\000\000\004\000\000\000\000\000\
\004\000\000\000\004\000\006\000\006\000\000\000\006\000\006\000\
\004\000\000\000\006\000\000\000\006\000\000\000\000\000\006\000\
\000\000\000\000\006\000\000\000\006\000\007\000\007\000\000\000\
\007\000\007\000\006\000\000\000\007\000\000\000\007\000\000\000\
\000\000\007\000\000\000\000\000\007\000\000\000\007\000\000\000\
\000\000\000\000\000\000\000\000\007\000"

let yycheck = "\001\001\
\000\000\000\000\017\001\007\000\006\000\013\000\017\001\005\000\
\001\000\016\001\012\000\017\001\013\001\014\001\007\000\000\000\
\027\001\025\001\033\001\021\000\028\001\028\001\020\000\021\000\
\000\000\027\000\016\001\033\001\032\000\000\000\001\001\039\000\
\000\000\000\000\000\000\001\001\002\001\001\000\028\001\032\000\
\033\000\017\001\044\000\045\000\046\000\047\000\048\000\049\000\
\050\000\053\000\054\000\002\001\003\001\051\000\052\000\013\001\
\014\001\000\000\024\001\061\000\053\000\054\000\001\001\001\001\
\002\001\000\000\000\000\025\001\016\001\025\001\008\001\009\001\
\028\001\039\001\012\001\003\001\004\001\005\001\006\001\007\001\
\006\001\007\001\010\001\011\001\092\000\001\001\024\001\035\001\
\036\001\037\001\000\000\002\001\003\001\035\001\096\000\097\000\
\038\001\025\001\000\000\000\000\104\000\039\001\018\001\035\001\
\106\000\021\001\038\001\023\001\013\001\014\001\026\001\104\000\
\013\001\014\001\019\001\031\001\032\001\022\001\019\001\121\000\
\025\001\022\001\030\001\000\000\025\001\127\000\003\001\004\001\
\005\001\006\001\007\001\000\000\000\000\013\001\013\001\014\001\
\000\000\030\001\038\001\019\001\013\001\014\001\022\001\022\001\
\019\001\025\001\019\001\022\001\025\001\035\001\025\001\003\001\
\004\001\005\001\006\001\007\001\000\000\000\000\010\001\011\001\
\003\001\004\001\005\001\006\001\007\001\035\001\035\001\024\001\
\038\001\038\001\005\001\006\001\007\001\001\001\030\001\035\001\
\001\001\001\001\001\001\001\001\001\001\001\001\000\000\030\001\
\030\001\035\001\028\001\035\001\014\001\034\001\029\001\029\001\
\002\001\001\001\001\001\001\001\001\001\029\001\016\001\028\001\
\030\001\020\001\035\001\002\001\002\001\001\001\001\001\000\000\
\000\000\255\255\255\255\038\001\029\001\029\001\028\001\030\001\
\016\001\029\001\029\001\255\255\255\255\030\001\035\001\255\255\
\255\255\000\000\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\000\000\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\003\001\004\001\005\001\006\001\007\001\
\255\255\255\255\010\001\011\001\255\255\013\001\014\001\255\255\
\255\255\017\001\017\001\019\001\255\255\255\255\022\001\255\255\
\255\255\025\001\255\255\027\001\027\001\255\255\255\255\255\255\
\017\001\033\001\033\001\035\001\003\001\004\001\005\001\006\001\
\007\001\017\001\027\001\010\001\011\001\255\255\013\001\014\001\
\033\001\017\001\017\001\027\001\019\001\255\255\255\255\022\001\
\027\001\033\001\025\001\027\001\027\001\027\001\033\001\255\255\
\255\255\033\001\033\001\033\001\035\001\003\001\004\001\005\001\
\006\001\007\001\017\001\255\255\010\001\011\001\255\255\013\001\
\014\001\255\255\017\001\017\001\027\001\019\001\255\255\255\255\
\022\001\255\255\033\001\025\001\027\001\027\001\255\255\255\255\
\255\255\255\255\033\001\033\001\255\255\035\001\003\001\004\001\
\005\001\006\001\007\001\017\001\255\255\010\001\011\001\255\255\
\013\001\014\001\255\255\017\001\017\001\027\001\019\001\255\255\
\255\255\022\001\255\255\033\001\025\001\027\001\027\001\255\255\
\255\255\255\255\255\255\033\001\033\001\255\255\035\001\003\001\
\004\001\005\001\006\001\007\001\017\001\255\255\010\001\011\001\
\255\255\013\001\014\001\255\255\017\001\017\001\027\001\019\001\
\255\255\017\001\022\001\255\255\033\001\025\001\027\001\027\001\
\003\001\004\001\005\001\027\001\033\001\033\001\255\255\010\001\
\011\001\033\001\013\001\014\001\255\255\017\001\017\001\255\255\
\019\001\255\255\255\255\022\001\255\255\255\255\025\001\027\001\
\027\001\003\001\004\001\255\255\255\255\033\001\033\001\255\255\
\010\001\011\001\255\255\013\001\014\001\255\255\255\255\017\001\
\255\255\019\001\255\255\255\255\022\001\255\255\255\255\025\001\
\255\255\027\001\003\001\004\001\255\255\255\255\255\255\033\001\
\255\255\010\001\011\001\255\255\013\001\014\001\255\255\255\255\
\017\001\255\255\019\001\255\255\255\255\022\001\255\255\255\255\
\025\001\255\255\027\001\010\001\011\001\255\255\013\001\014\001\
\033\001\255\255\017\001\255\255\019\001\255\255\255\255\022\001\
\255\255\255\255\025\001\255\255\027\001\010\001\011\001\255\255\
\013\001\014\001\033\001\255\255\017\001\255\255\019\001\255\255\
\255\255\022\001\255\255\255\255\025\001\255\255\027\001\255\255\
\255\255\255\255\255\255\255\255\033\001"

let yynames_const = "\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIV\000\
  MOD\000\
  TRUE\000\
  FALSE\000\
  EQ_TOK\000\
  LE_TOK\000\
  NOT\000\
  AND\000\
  OR\000\
  SKIP\000\
  SET\000\
  SEMICOLON\000\
  IF\000\
  THEN\000\
  ELSE\000\
  WHILE\000\
  DO\000\
  PRINT\000\
  LPAREN\000\
  RPAREN\000\
  LBRACE\000\
  RBRACE\000\
  LBRACK\000\
  RBRACK\000\
  QUOTE\000\
  DECLAREINT\000\
  DECLAREPROTO\000\
  END\000\
  PERIOD\000\
  DOLLAR\000\
  READFROM\000\
  WRITETO\000\
  COPYFROM\000\
  SIZEOF\000\
  EOF\000\
  "

let yynames_block = "\
  IDENTIFIER\000\
  INT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 74 "parse.mly"
                                             ( Const(_1) )
# 376 "parse.ml"
               : 'aexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 75 "parse.mly"
                                             ( Var _1)
# 383 "parse.ml"
               : 'aexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'aexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aexp) in
    Obj.repr(
# 76 "parse.mly"
                                             ( Add (_1,_3) )
# 391 "parse.ml"
               : 'aexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'aexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aexp) in
    Obj.repr(
# 77 "parse.mly"
                                             ( Sub (_1,_3) )
# 399 "parse.ml"
               : 'aexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'aexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aexp) in
    Obj.repr(
# 78 "parse.mly"
                                             ( Mul (_1,_3) )
# 407 "parse.ml"
               : 'aexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'aexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aexp) in
    Obj.repr(
# 79 "parse.mly"
                                             ( Div (_1,_3) )
# 415 "parse.ml"
               : 'aexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'aexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aexp) in
    Obj.repr(
# 80 "parse.mly"
                                             ( Mod (_1,_3) )
# 423 "parse.ml"
               : 'aexp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'aexp) in
    Obj.repr(
# 81 "parse.mly"
                                             ( _2 )
# 430 "parse.ml"
               : 'aexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'pro_ele) in
    Obj.repr(
# 82 "parse.mly"
                                             ( Proto _1)
# 437 "parse.ml"
               : 'aexp))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : 'exp_inside) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 83 "parse.mly"
                                                                       (  Sizeof1(_3,_5,_7) )
# 446 "parse.ml"
               : 'aexp))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 84 "parse.mly"
                                                     (  Sizeof2(_3,_5) )
# 454 "parse.ml"
               : 'aexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 87 "parse.mly"
                               (Var _1)
# 461 "parse.ml"
               : 'pro_ele))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'exp_inside) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 88 "parse.mly"
                                                                      (     AccessProto1(_1,_3,_5) )
# 470 "parse.ml"
               : 'pro_ele))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'exp_inside) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 89 "parse.mly"
                                                                      (     AccessProto2(_1,_3,_5,_7) )
# 480 "parse.ml"
               : 'pro_ele))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 90 "parse.mly"
                                                     ( AccessProto3(_1,_3) )
# 488 "parse.ml"
               : 'pro_ele))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 91 "parse.mly"
                                                 ( AccessProto4(_1,_3,_5) )
# 497 "parse.ml"
               : 'pro_ele))
; (fun __caml_parser_env ->
    Obj.repr(
# 94 "parse.mly"
                                             ( True )
# 503 "parse.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    Obj.repr(
# 95 "parse.mly"
                                             ( False )
# 509 "parse.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'aexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aexp) in
    Obj.repr(
# 96 "parse.mly"
                                             ( EQ (_1,_3) )
# 517 "parse.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'aexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aexp) in
    Obj.repr(
# 97 "parse.mly"
                                             ( LE(_1,_3) )
# 525 "parse.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'bexp) in
    Obj.repr(
# 98 "parse.mly"
                                             ( Not(_2) )
# 532 "parse.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'bexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'bexp) in
    Obj.repr(
# 99 "parse.mly"
                                             ( And(_1,_3)  )
# 540 "parse.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'bexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'bexp) in
    Obj.repr(
# 100 "parse.mly"
                                             ( Or(_1,_3) )
# 548 "parse.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'bexp) in
    Obj.repr(
# 101 "parse.mly"
                                             ( _2 )
# 555 "parse.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aexp) in
    Obj.repr(
# 105 "parse.mly"
                                             ( Set(_1,_3) )
# 563 "parse.ml"
               : Imp.com))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Imp.com) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Imp.com) in
    Obj.repr(
# 106 "parse.mly"
                                             ( Seq(_1,_3) )
# 571 "parse.ml"
               : Imp.com))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'bexp) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Imp.com) in
    Obj.repr(
# 107 "parse.mly"
                              ( If(_2,_4) )
# 579 "parse.ml"
               : Imp.com))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : 'bexp) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : Imp.com) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : Imp.com) in
    Obj.repr(
# 108 "parse.mly"
                                             ( Ifelse(_2,_4,_7) )
# 588 "parse.ml"
               : Imp.com))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'bexp) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Imp.com) in
    Obj.repr(
# 109 "parse.mly"
                                             ( While(_2,_4) )
# 596 "parse.ml"
               : Imp.com))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'aexp) in
    Obj.repr(
# 110 "parse.mly"
                                             ( Print(_2) )
# 603 "parse.ml"
               : Imp.com))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Imp.com) in
    Obj.repr(
# 111 "parse.mly"
                                             ( _2 )
# 610 "parse.ml"
               : Imp.com))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 113 "parse.mly"
                               ( Declareint(_2) )
# 617 "parse.ml"
               : Imp.com))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 8 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _9 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 114 "parse.mly"
                                                                                          ( Declareproto(_2,_4,_9) )
# 627 "parse.ml"
               : Imp.com))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 115 "parse.mly"
                                                ( Readfrom(_1,_4) )
# 635 "parse.ml"
               : Imp.com))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 116 "parse.mly"
                                                ( Writeto(_1,_4) )
# 643 "parse.ml"
               : Imp.com))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'exp_inside) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'aexp) in
    Obj.repr(
# 118 "parse.mly"
                                                                                   ( SetProto1(_1,_3,_5,_7) )
# 653 "parse.ml"
               : Imp.com))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 9 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 7 : 'exp_inside) in
    let _5 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _7 = (Parsing.peek_val __caml_parser_env 3 : int) in
    let _10 = (Parsing.peek_val __caml_parser_env 0 : 'aexp) in
    Obj.repr(
# 119 "parse.mly"
                                                                               ( SetProto2(_1,_3,_5,_7,_10) )
# 664 "parse.ml"
               : Imp.com))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'aexp) in
    Obj.repr(
# 120 "parse.mly"
                                                                        (     SetProto3(_1,_3,_5) )
# 673 "parse.ml"
               : Imp.com))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : int) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'aexp) in
    Obj.repr(
# 121 "parse.mly"
                                                                        (     SetProto4(_1,_3,_5,_8) )
# 683 "parse.ml"
               : Imp.com))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'aexp) in
    Obj.repr(
# 123 "parse.mly"
                                                       (AddEle1(_1,_3,_7))
# 692 "parse.ml"
               : Imp.com))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 8 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'exp_inside) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'aexp) in
    Obj.repr(
# 124 "parse.mly"
                                                                         (AddEle2(_1,_3,_5,_9))
# 702 "parse.ml"
               : Imp.com))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string) in
    Obj.repr(
# 125 "parse.mly"
                                                  (AddEle3(_1,_3))
# 710 "parse.ml"
               : Imp.com))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'exp_inside) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : string) in
    Obj.repr(
# 126 "parse.mly"
                                                                     (AddEle4(_1,_3,_5))
# 719 "parse.ml"
               : Imp.com))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'pro_ele) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'pro_ele) in
    Obj.repr(
# 127 "parse.mly"
                           ( Copyfrom(_1,_3) )
# 727 "parse.ml"
               : Imp.com))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 132 "parse.mly"
              ( ExpEle1 _1 )
# 734 "parse.ml"
               : 'exp_inside))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 133 "parse.mly"
                                ( ExpEle2 (_1, _3) )
# 742 "parse.ml"
               : 'exp_inside))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp_inside) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp_inside) in
    Obj.repr(
# 134 "parse.mly"
                                       ( ExpEle3 (_1, _3) )
# 750 "parse.ml"
               : 'exp_inside))
(* Entry com *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let com (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Imp.com)
