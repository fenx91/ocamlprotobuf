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

# 56 "parse.ml"
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
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* IDENTIFIER *);
  258 (* INT *);
    0|]

let yylhs = "\255\255\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\004\000\004\000\
\004\000\000\000"

let yylen = "\002\000\
\001\000\001\000\003\000\003\000\003\000\003\000\003\000\003\000\
\001\000\001\000\003\000\003\000\002\000\003\000\003\000\003\000\
\003\000\003\000\005\000\008\000\004\000\002\000\003\000\002\000\
\010\000\005\000\005\000\006\000\005\000\008\000\002\000\004\000\
\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\001\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\002\000\009\000\010\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\024\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\013\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\008\000\023\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\031\000\000\000\000\000\
\000\000\000\000\016\000\000\000\000\000\000\000\015\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\026\000\
\027\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\032\000\000\000\000\000\000\000\020\000\000\000\000\000\
\025\000"

let yydgoto = "\002\000\
\012\000\013\000\023\000\064\000"

let yysindex = "\037\000\
\078\255\000\000\131\255\000\000\044\255\044\255\005\255\005\255\
\078\255\001\255\019\255\038\255\079\255\056\255\028\255\037\255\
\000\000\000\000\000\000\044\255\044\255\149\255\126\255\129\255\
\158\255\006\255\253\254\000\000\040\255\078\255\005\255\005\255\
\005\255\005\255\005\255\005\255\244\254\070\255\076\255\080\255\
\000\000\087\255\123\255\005\255\005\255\044\255\044\255\078\255\
\078\255\000\000\000\000\099\255\038\255\164\255\164\255\021\255\
\158\255\158\255\158\255\005\255\086\255\000\000\031\255\102\255\
\075\255\077\255\000\000\158\255\158\255\094\255\000\000\008\255\
\038\255\089\255\158\255\082\255\005\255\122\255\243\254\000\000\
\000\000\112\255\133\255\128\255\158\255\117\255\078\255\119\255\
\005\255\000\000\015\255\120\255\158\255\000\000\146\255\127\255\
\000\000"

let yyrindex = "\000\000\
\000\000\000\000\110\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\158\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\005\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\019\000\026\000\051\000\001\000\
\069\000\087\000\008\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\106\255\108\255\116\255\000\000\000\000\
\042\000\000\000\030\000\000\000\000\000\000\000\000\000\000\000\
\000\000\033\000\000\000\171\255\054\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\072\000\000\000\000\000\000\000\
\000\000"

let yygindex = "\000\000\
\247\255\029\000\253\255\159\000"

let yytablesize = 376
let yytable = "\027\000\
\005\000\028\000\024\000\060\000\022\000\017\000\004\000\017\000\
\031\000\032\000\033\000\034\000\035\000\030\000\078\000\061\000\
\041\000\043\000\018\000\029\000\053\000\062\000\062\000\051\000\
\030\000\003\000\034\000\035\000\008\000\029\000\050\000\030\000\
\019\000\022\000\022\000\025\000\026\000\001\000\072\000\073\000\
\082\000\021\000\070\000\071\000\017\000\004\000\077\000\094\000\
\022\000\042\000\004\000\018\000\019\000\028\000\030\000\020\000\
\037\000\039\000\078\000\054\000\055\000\056\000\057\000\058\000\
\059\000\062\000\040\000\021\000\006\000\052\000\063\000\030\000\
\068\000\069\000\022\000\022\000\065\000\091\000\003\000\004\000\
\066\000\031\000\032\000\033\000\034\000\035\000\007\000\076\000\
\075\000\031\000\032\000\033\000\034\000\035\000\036\000\005\000\
\044\000\045\000\006\000\074\000\007\000\008\000\079\000\009\000\
\080\000\085\000\081\000\047\000\010\000\011\000\084\000\050\000\
\002\000\002\000\002\000\002\000\002\000\093\000\011\000\011\000\
\012\000\012\000\083\000\086\000\011\000\002\000\012\000\011\000\
\014\000\012\000\011\000\087\000\012\000\088\000\014\000\046\000\
\047\000\014\000\046\000\047\000\014\000\046\000\047\000\089\000\
\048\000\090\000\096\000\067\000\092\000\095\000\049\000\031\000\
\032\000\033\000\034\000\035\000\097\000\034\000\044\000\045\000\
\031\000\032\000\033\000\034\000\035\000\014\000\015\000\016\000\
\033\000\034\000\035\000\032\000\038\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\005\000\005\000\005\000\000\000\000\000\
\000\000\000\000\005\000\005\000\000\000\005\000\005\000\000\000\
\005\000\005\000\000\000\005\000\000\000\022\000\005\000\000\000\
\017\000\005\000\000\000\005\000\003\000\003\000\000\000\022\000\
\000\000\005\000\017\000\003\000\003\000\022\000\003\000\003\000\
\017\000\003\000\003\000\000\000\003\000\018\000\029\000\003\000\
\000\000\019\000\003\000\018\000\003\000\004\000\004\000\000\000\
\029\000\000\000\003\000\019\000\004\000\004\000\029\000\004\000\
\004\000\019\000\004\000\004\000\021\000\004\000\028\000\000\000\
\004\000\000\000\021\000\004\000\000\000\004\000\006\000\006\000\
\028\000\006\000\006\000\004\000\006\000\006\000\028\000\006\000\
\030\000\000\000\006\000\000\000\000\000\006\000\000\000\006\000\
\007\000\007\000\030\000\007\000\007\000\006\000\007\000\007\000\
\030\000\007\000\000\000\000\000\007\000\000\000\000\000\007\000\
\000\000\007\000\000\000\000\000\000\000\000\000\000\000\007\000"

let yycheck = "\009\000\
\000\000\001\001\006\000\016\001\000\000\001\001\002\001\000\000\
\003\001\004\001\005\001\006\001\007\001\017\001\028\001\028\001\
\020\000\021\000\000\000\001\001\030\000\035\001\035\001\027\001\
\017\001\000\000\006\001\007\001\024\001\000\000\025\001\017\001\
\000\000\005\000\006\000\007\000\008\000\001\000\048\000\049\000\
\033\001\000\000\046\000\047\000\001\001\002\001\016\001\033\001\
\020\000\021\000\000\000\008\001\009\001\000\000\017\001\012\001\
\001\001\030\001\028\001\031\000\032\000\033\000\034\000\035\000\
\036\000\035\001\030\001\024\001\000\000\030\001\001\001\000\000\
\044\000\045\000\046\000\047\000\001\001\087\000\001\001\002\001\
\001\001\003\001\004\001\005\001\006\001\007\001\000\000\002\001\
\060\000\003\001\004\001\005\001\006\001\007\001\016\001\018\001\
\010\001\011\001\021\001\001\001\023\001\024\001\001\001\026\001\
\030\001\077\000\030\001\014\001\031\001\032\001\029\001\025\001\
\003\001\004\001\005\001\006\001\007\001\089\000\013\001\014\001\
\013\001\014\001\034\001\002\001\019\001\016\001\019\001\022\001\
\013\001\022\001\025\001\020\001\025\001\001\001\019\001\013\001\
\014\001\022\001\013\001\014\001\025\001\013\001\014\001\016\001\
\019\001\029\001\001\001\025\001\030\001\030\001\022\001\003\001\
\004\001\005\001\006\001\007\001\030\001\000\000\010\001\011\001\
\003\001\004\001\005\001\006\001\007\001\035\001\036\001\037\001\
\005\001\006\001\007\001\001\001\014\000\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\003\001\004\001\005\001\255\255\255\255\
\255\255\255\255\010\001\011\001\255\255\013\001\014\001\255\255\
\016\001\017\001\255\255\019\001\255\255\017\001\022\001\255\255\
\017\001\025\001\255\255\027\001\003\001\004\001\255\255\027\001\
\255\255\033\001\027\001\010\001\011\001\033\001\013\001\014\001\
\033\001\016\001\017\001\255\255\019\001\027\001\017\001\022\001\
\255\255\017\001\025\001\033\001\027\001\003\001\004\001\255\255\
\027\001\255\255\033\001\027\001\010\001\011\001\033\001\013\001\
\014\001\033\001\016\001\017\001\027\001\019\001\017\001\255\255\
\022\001\255\255\033\001\025\001\255\255\027\001\010\001\011\001\
\027\001\013\001\014\001\033\001\016\001\017\001\033\001\019\001\
\017\001\255\255\022\001\255\255\255\255\025\001\255\255\027\001\
\010\001\011\001\027\001\013\001\014\001\033\001\016\001\017\001\
\033\001\019\001\255\255\255\255\022\001\255\255\255\255\025\001\
\255\255\027\001\255\255\255\255\255\255\255\255\255\255\033\001"

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
# 67 "parse.mly"
                                             ( Const(_1) )
# 316 "parse.ml"
               : 'aexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 68 "parse.mly"
                                             ( Var _1)
# 323 "parse.ml"
               : 'aexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'aexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aexp) in
    Obj.repr(
# 69 "parse.mly"
                                             ( Add (_1,_3) )
# 331 "parse.ml"
               : 'aexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'aexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aexp) in
    Obj.repr(
# 70 "parse.mly"
                                             ( Sub (_1,_3) )
# 339 "parse.ml"
               : 'aexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'aexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aexp) in
    Obj.repr(
# 71 "parse.mly"
                                             ( Mul (_1,_3) )
# 347 "parse.ml"
               : 'aexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'aexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aexp) in
    Obj.repr(
# 72 "parse.mly"
                                             ( Div (_1,_3) )
# 355 "parse.ml"
               : 'aexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'aexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aexp) in
    Obj.repr(
# 73 "parse.mly"
                                             ( Mod (_1,_3) )
# 363 "parse.ml"
               : 'aexp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'aexp) in
    Obj.repr(
# 74 "parse.mly"
                                             ( _2 )
# 370 "parse.ml"
               : 'aexp))
; (fun __caml_parser_env ->
    Obj.repr(
# 77 "parse.mly"
                                             ( True )
# 376 "parse.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    Obj.repr(
# 78 "parse.mly"
                                             ( False )
# 382 "parse.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'aexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aexp) in
    Obj.repr(
# 79 "parse.mly"
                                             ( EQ (_1,_3) )
# 390 "parse.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'aexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aexp) in
    Obj.repr(
# 80 "parse.mly"
                                             ( LE(_1,_3) )
# 398 "parse.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'bexp) in
    Obj.repr(
# 81 "parse.mly"
                                             ( Not(_2) )
# 405 "parse.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'bexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'bexp) in
    Obj.repr(
# 82 "parse.mly"
                                             ( And(_1,_3)  )
# 413 "parse.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'bexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'bexp) in
    Obj.repr(
# 83 "parse.mly"
                                             ( Or(_1,_3) )
# 421 "parse.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'bexp) in
    Obj.repr(
# 84 "parse.mly"
                                             ( _2 )
# 428 "parse.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'aexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aexp) in
    Obj.repr(
# 88 "parse.mly"
                                       ( Set(_1,_3) )
# 436 "parse.ml"
               : Imp.com))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Imp.com) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Imp.com) in
    Obj.repr(
# 89 "parse.mly"
                                             ( Seq(_1,_3) )
# 444 "parse.ml"
               : Imp.com))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'bexp) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Imp.com) in
    Obj.repr(
# 90 "parse.mly"
                              ( If(_2,_4) )
# 452 "parse.ml"
               : Imp.com))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : 'bexp) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : Imp.com) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : Imp.com) in
    Obj.repr(
# 91 "parse.mly"
                                             ( Ifelse(_2,_4,_7) )
# 461 "parse.ml"
               : Imp.com))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'bexp) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Imp.com) in
    Obj.repr(
# 92 "parse.mly"
                                             ( While(_2,_4) )
# 469 "parse.ml"
               : Imp.com))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'aexp) in
    Obj.repr(
# 93 "parse.mly"
                                             ( Print(_2) )
# 476 "parse.ml"
               : Imp.com))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Imp.com) in
    Obj.repr(
# 94 "parse.mly"
                                             ( _2 )
# 483 "parse.ml"
               : Imp.com))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 95 "parse.mly"
                               ( Declareint(_2) )
# 490 "parse.ml"
               : Imp.com))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 8 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _9 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 96 "parse.mly"
                                                                                          ( Declareproto(_2,_4,_9) )
# 500 "parse.ml"
               : Imp.com))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 97 "parse.mly"
                                                ( Readfrom(_1,_4) )
# 508 "parse.ml"
               : Imp.com))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 98 "parse.mly"
                                                ( Writeto(_1,_4) )
# 516 "parse.ml"
               : Imp.com))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'exp_inside) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'aexp) in
    Obj.repr(
# 99 "parse.mly"
                                                                        ( SetProto1(_1,_3,_4,_6) )
# 526 "parse.ml"
               : Imp.com))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'aexp) in
    Obj.repr(
# 100 "parse.mly"
                                                                        ( SetProto3(_1,_3,_5) )
# 535 "parse.ml"
               : Imp.com))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : int) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'aexp) in
    Obj.repr(
# 101 "parse.mly"
                                                                        ( SetProto4(_1,_3,_5,_8) )
# 545 "parse.ml"
               : Imp.com))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 105 "parse.mly"
                                ( ExpInside1 _1 )
# 552 "parse.ml"
               : 'exp_inside))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 106 "parse.mly"
                                ( ExpInside2 (_1, _3) )
# 560 "parse.ml"
               : 'exp_inside))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'exp_inside) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'exp_inside) in
    Obj.repr(
# 107 "parse.mly"
                                ( ExpInside3 (_1, S2) :)
# 568 "parse.ml"
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
