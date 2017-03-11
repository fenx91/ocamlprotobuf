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

# 62 "parse.ml"
let yytransl_const = [|
  259 (* PLUS *);
  260 (* MINUS *);
  261 (* TIMES *);
  262 (* DIV *);
  263 (* MOD *);
  264 (* TRUE *);
  265 (* FALSE *);
  266 (* EQ_TOK *);
  267 (* L_TOK *);
  268 (* LE_TOK *);
  269 (* B_TOK *);
  270 (* BE_TOK *);
  271 (* NOT *);
  272 (* AND *);
  273 (* OR *);
  274 (* SKIP *);
  275 (* SET *);
  276 (* SEMICOLON *);
  277 (* IF *);
  278 (* THEN *);
  279 (* ELSE *);
  280 (* WHILE *);
  281 (* DO *);
  282 (* PRINT *);
  283 (* LPAREN *);
  284 (* RPAREN *);
  285 (* LBRACE *);
  286 (* RBRACE *);
  287 (* LBRACK *);
  288 (* RBRACK *);
  289 (* QUOTE *);
  290 (* DECLAREINT *);
  291 (* DECLAREPROTO *);
  292 (* DECLAREBOOL *);
  293 (* END *);
  294 (* PERIOD *);
  295 (* DOLLAR *);
  296 (* READFROM *);
  297 (* WRITETO *);
  298 (* COPYFROM *);
  299 (* SIZEOF *);
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
\005\000\005\000\005\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\004\000\004\000\004\000\000\000"

let yylen = "\002\000\
\001\000\001\000\003\000\003\000\003\000\003\000\003\000\003\000\
\001\000\008\000\006\000\001\000\005\000\008\000\003\000\006\000\
\001\000\001\000\003\000\003\000\003\000\003\000\003\000\002\000\
\003\000\003\000\003\000\003\000\003\000\003\000\005\000\008\000\
\004\000\002\000\003\000\002\000\002\000\010\000\005\000\005\000\
\007\000\010\000\005\000\008\000\007\000\009\000\006\000\008\000\
\003\000\001\000\004\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\001\000\017\000\018\000\000\000\000\000\000\000\000\000\
\009\000\000\000\000\000\000\000\000\000\000\000\036\000\000\000\
\037\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\035\000\000\000\000\000\
\000\000\049\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\008\000\027\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\026\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\052\000\
\039\000\040\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\011\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\032\000\000\000\000\000\
\000\000\000\000\051\000\010\000\000\000\000\000\038\000"

let yydgoto = "\002\000\
\011\000\024\000\025\000\096\000\026\000"

let yysindex = "\013\000\
\061\255\000\000\074\255\009\255\009\255\024\255\061\255\007\255\
\031\255\036\255\053\255\047\255\009\255\093\255\051\255\065\255\
\082\255\000\000\000\000\000\000\009\255\009\255\096\255\152\255\
\000\000\018\255\052\255\024\255\113\255\245\254\000\000\099\255\
\000\000\061\255\128\255\152\255\090\255\240\254\109\255\136\255\
\166\255\168\255\090\255\140\255\042\255\169\255\024\255\024\255\
\024\255\024\255\024\255\024\255\024\255\024\255\024\255\024\255\
\009\255\009\255\061\255\061\255\098\255\000\000\170\255\053\255\
\082\255\000\000\024\255\137\255\171\255\141\255\142\255\147\255\
\134\255\000\000\000\000\143\255\129\255\129\255\135\255\113\255\
\113\255\113\255\113\255\113\255\113\255\113\255\162\255\000\000\
\242\254\053\255\145\255\113\255\148\255\149\255\041\255\000\000\
\000\000\000\000\182\255\184\255\185\255\164\255\187\255\172\255\
\024\255\024\255\158\255\188\255\160\255\159\255\035\255\154\255\
\061\255\161\255\024\255\113\255\113\255\163\255\165\255\167\255\
\000\000\194\255\000\000\197\255\199\255\249\254\173\255\113\255\
\183\255\024\255\175\255\176\255\050\255\000\000\200\255\024\255\
\113\255\000\000\000\000\000\000\177\255\113\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\174\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\203\000\000\000\000\000\000\000\000\000\000\000\
\149\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\002\000\000\000\000\000\000\000\
\000\000\000\000\000\000\053\000\074\000\044\255\000\000\000\000\
\000\000\000\000\039\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\071\000\
\076\000\000\000\000\000\000\000\000\000\000\000\000\000\001\000\
\000\000\000\000\000\000\000\000\205\000\233\000\177\000\255\000\
\027\001\046\001\062\001\084\001\102\001\120\001\111\000\000\000\
\000\000\108\000\000\000\127\000\000\000\000\000\049\255\000\000\
\000\000\000\000\000\000\000\000\000\000\138\000\000\000\083\255\
\176\000\000\000\000\000\000\000\000\000\038\000\178\255\000\000\
\000\000\000\000\000\000\204\000\244\000\000\000\000\000\178\255\
\075\000\000\000\000\000\000\000\000\000\000\000\000\000\028\001\
\089\255\030\001\000\000\000\000\178\255\000\000\000\000\000\000\
\065\001\112\000\000\000\000\000\000\000\121\001\000\000"

let yygindex = "\000\000\
\253\255\250\255\020\000\247\255\007\000"

let yytablesize = 670
let yytable = "\029\000\
\015\000\034\000\067\000\030\000\039\000\034\000\036\000\031\000\
\034\000\017\000\018\000\027\000\034\000\001\000\068\000\044\000\
\019\000\020\000\062\000\037\000\012\000\061\000\102\000\021\000\
\017\000\018\000\012\000\043\000\045\000\134\000\064\000\032\000\
\073\000\057\000\058\000\022\000\033\000\013\000\024\000\059\000\
\077\000\078\000\079\000\080\000\081\000\082\000\083\000\084\000\
\085\000\086\000\028\000\023\000\028\000\012\000\066\000\089\000\
\090\000\057\000\058\000\106\000\092\000\003\000\123\000\087\000\
\088\000\124\000\023\000\057\000\058\000\075\000\030\000\107\000\
\034\000\029\000\016\000\012\000\060\000\140\000\012\000\012\000\
\124\000\004\000\050\000\040\000\005\000\015\000\006\000\050\000\
\035\000\007\000\013\000\112\000\013\000\038\000\008\000\009\000\
\010\000\041\000\116\000\117\000\047\000\048\000\049\000\050\000\
\051\000\057\000\058\000\033\000\128\000\126\000\025\000\014\000\
\014\000\015\000\016\000\047\000\048\000\049\000\050\000\051\000\
\042\000\051\000\046\000\137\000\016\000\074\000\043\000\051\000\
\065\000\142\000\014\000\063\000\012\000\049\000\050\000\051\000\
\070\000\031\000\093\000\094\000\050\000\051\000\047\000\048\000\
\049\000\050\000\051\000\069\000\002\000\052\000\053\000\054\000\
\055\000\056\000\047\000\048\000\049\000\050\000\051\000\118\000\
\119\000\052\000\053\000\054\000\055\000\056\000\071\000\074\000\
\072\000\076\000\091\000\095\000\100\000\097\000\098\000\047\000\
\005\000\099\000\058\000\104\000\105\000\101\000\103\000\109\000\
\110\000\111\000\113\000\114\000\120\000\122\000\115\000\121\000\
\125\000\127\000\129\000\131\000\130\000\124\000\132\000\133\000\
\141\000\136\000\053\000\045\000\003\000\135\000\138\000\139\000\
\000\000\143\000\000\000\000\000\000\000\000\000\000\000\012\000\
\050\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\004\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\041\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\006\000\000\000\
\000\000\000\000\000\000\015\000\015\000\015\000\015\000\015\000\
\000\000\000\000\015\000\015\000\015\000\015\000\015\000\000\000\
\015\000\015\000\000\000\000\000\015\000\034\000\015\000\000\000\
\000\000\015\000\007\000\044\000\015\000\048\000\015\000\034\000\
\000\000\000\000\000\000\000\000\000\000\015\000\034\000\050\000\
\013\000\013\000\013\000\013\000\013\000\019\000\000\000\013\000\
\013\000\013\000\013\000\013\000\000\000\013\000\013\000\000\000\
\000\000\013\000\024\000\013\000\024\000\021\000\013\000\024\000\
\046\000\013\000\024\000\013\000\024\000\000\000\000\000\000\000\
\028\000\000\000\013\000\024\000\050\000\016\000\016\000\016\000\
\016\000\016\000\028\000\020\000\016\000\016\000\016\000\016\000\
\016\000\028\000\016\000\016\000\000\000\029\000\016\000\012\000\
\016\000\000\000\000\000\016\000\030\000\022\000\016\000\029\000\
\016\000\012\000\000\000\030\000\000\000\000\000\029\000\016\000\
\012\000\051\000\014\000\014\000\014\000\014\000\014\000\023\000\
\042\000\014\000\014\000\014\000\014\000\014\000\025\000\014\000\
\014\000\000\000\025\000\014\000\025\000\014\000\000\000\025\000\
\014\000\033\000\025\000\014\000\025\000\014\000\000\000\000\000\
\033\000\000\000\043\000\025\000\014\000\000\000\051\000\002\000\
\002\000\002\000\002\000\002\000\043\000\031\000\002\000\002\000\
\002\000\002\000\002\000\043\000\002\000\002\000\000\000\031\000\
\002\000\000\000\002\000\000\000\000\000\002\000\031\000\000\000\
\002\000\000\000\002\000\005\000\005\000\005\000\000\000\000\000\
\000\000\002\000\005\000\005\000\005\000\005\000\005\000\000\000\
\005\000\005\000\000\000\047\000\005\000\000\000\005\000\000\000\
\000\000\005\000\000\000\000\000\005\000\047\000\005\000\003\000\
\003\000\000\000\000\000\000\000\047\000\005\000\003\000\003\000\
\003\000\003\000\003\000\000\000\003\000\003\000\000\000\045\000\
\003\000\000\000\003\000\000\000\000\000\003\000\000\000\000\000\
\003\000\045\000\003\000\004\000\004\000\000\000\000\000\000\000\
\045\000\003\000\004\000\004\000\004\000\004\000\004\000\000\000\
\004\000\004\000\000\000\000\000\004\000\000\000\004\000\000\000\
\000\000\004\000\000\000\000\000\004\000\000\000\004\000\041\000\
\006\000\006\000\006\000\006\000\006\000\004\000\006\000\006\000\
\000\000\041\000\006\000\000\000\006\000\000\000\000\000\006\000\
\041\000\000\000\006\000\000\000\006\000\000\000\000\000\000\000\
\000\000\000\000\000\000\006\000\007\000\007\000\007\000\007\000\
\007\000\000\000\007\000\007\000\000\000\000\000\007\000\044\000\
\007\000\048\000\000\000\007\000\000\000\000\000\007\000\000\000\
\007\000\044\000\000\000\048\000\000\000\019\000\019\000\007\000\
\044\000\019\000\048\000\019\000\000\000\000\000\019\000\000\000\
\000\000\019\000\000\000\019\000\000\000\021\000\021\000\000\000\
\000\000\021\000\019\000\021\000\046\000\000\000\021\000\000\000\
\000\000\021\000\000\000\021\000\000\000\000\000\046\000\000\000\
\000\000\000\000\021\000\020\000\020\000\046\000\000\000\020\000\
\000\000\020\000\000\000\000\000\020\000\000\000\000\000\020\000\
\000\000\020\000\000\000\000\000\000\000\022\000\022\000\000\000\
\020\000\022\000\000\000\022\000\000\000\000\000\022\000\000\000\
\000\000\022\000\000\000\022\000\000\000\000\000\000\000\023\000\
\023\000\000\000\022\000\023\000\042\000\023\000\000\000\000\000\
\023\000\000\000\000\000\023\000\000\000\023\000\042\000\000\000\
\000\000\000\000\000\000\000\000\023\000\042\000"

let yycheck = "\006\000\
\000\000\000\000\019\001\007\000\014\000\020\001\013\000\001\001\
\020\001\001\001\002\001\005\000\020\001\001\000\031\001\022\000\
\008\001\009\001\030\001\013\000\001\000\028\000\037\001\015\001\
\001\001\002\001\007\000\021\000\022\000\037\001\034\000\001\001\
\042\000\016\001\017\001\027\001\001\001\000\000\000\000\022\001\
\047\000\048\000\049\000\050\000\051\000\052\000\053\000\054\000\
\055\000\056\000\027\001\043\001\000\000\034\000\035\000\059\000\
\060\000\016\001\017\001\019\001\067\000\001\001\028\001\057\000\
\058\000\031\001\043\001\016\001\017\001\028\001\000\000\031\001\
\020\001\000\000\000\000\000\000\025\001\028\001\059\000\060\000\
\031\001\021\001\039\001\033\001\024\001\042\001\026\001\039\001\
\042\001\029\001\042\001\101\000\019\001\001\001\034\001\035\001\
\036\001\033\001\105\000\106\000\003\001\004\001\005\001\006\001\
\007\001\016\001\017\001\000\000\115\000\113\000\000\000\000\000\
\039\001\040\001\041\001\003\001\004\001\005\001\006\001\007\001\
\039\001\039\001\027\001\130\000\042\001\028\001\000\000\039\001\
\001\001\136\000\042\001\033\001\113\000\005\001\006\001\007\001\
\001\001\000\000\002\001\003\001\006\001\007\001\003\001\004\001\
\005\001\006\001\007\001\039\001\000\000\010\001\011\001\012\001\
\013\001\014\001\003\001\004\001\005\001\006\001\007\001\002\001\
\003\001\010\001\011\001\012\001\013\001\014\001\001\001\028\001\
\001\001\001\001\001\001\001\001\039\001\033\001\033\001\000\000\
\000\000\031\001\017\001\032\001\032\001\039\001\038\001\002\001\
\001\001\001\001\023\001\001\001\001\001\031\001\019\001\032\001\
\039\001\033\001\032\001\002\001\032\001\031\001\002\001\001\001\
\001\001\019\001\000\000\000\000\000\000\033\001\032\001\032\001\
\255\255\033\001\255\255\255\255\255\255\255\255\255\255\042\001\
\039\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\000\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\000\000\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\000\000\255\255\
\255\255\255\255\255\255\003\001\004\001\005\001\006\001\007\001\
\255\255\255\255\010\001\011\001\012\001\013\001\014\001\255\255\
\016\001\017\001\255\255\255\255\020\001\020\001\022\001\255\255\
\255\255\025\001\000\000\000\000\028\001\000\000\030\001\030\001\
\255\255\255\255\255\255\255\255\255\255\037\001\037\001\039\001\
\003\001\004\001\005\001\006\001\007\001\000\000\255\255\010\001\
\011\001\012\001\013\001\014\001\255\255\016\001\017\001\255\255\
\255\255\020\001\020\001\022\001\022\001\000\000\025\001\025\001\
\000\000\028\001\028\001\030\001\030\001\255\255\255\255\255\255\
\020\001\255\255\037\001\037\001\039\001\003\001\004\001\005\001\
\006\001\007\001\030\001\000\000\010\001\011\001\012\001\013\001\
\014\001\037\001\016\001\017\001\255\255\020\001\020\001\020\001\
\022\001\255\255\255\255\025\001\030\001\000\000\028\001\030\001\
\030\001\030\001\255\255\037\001\255\255\255\255\037\001\037\001\
\037\001\039\001\003\001\004\001\005\001\006\001\007\001\000\000\
\000\000\010\001\011\001\012\001\013\001\014\001\016\001\016\001\
\017\001\255\255\020\001\020\001\022\001\022\001\255\255\025\001\
\025\001\030\001\028\001\028\001\030\001\030\001\255\255\255\255\
\037\001\255\255\020\001\037\001\037\001\255\255\039\001\003\001\
\004\001\005\001\006\001\007\001\030\001\020\001\010\001\011\001\
\012\001\013\001\014\001\037\001\016\001\017\001\255\255\030\001\
\020\001\255\255\022\001\255\255\255\255\025\001\037\001\255\255\
\028\001\255\255\030\001\003\001\004\001\005\001\255\255\255\255\
\255\255\037\001\010\001\011\001\012\001\013\001\014\001\255\255\
\016\001\017\001\255\255\020\001\020\001\255\255\022\001\255\255\
\255\255\025\001\255\255\255\255\028\001\030\001\030\001\003\001\
\004\001\255\255\255\255\255\255\037\001\037\001\010\001\011\001\
\012\001\013\001\014\001\255\255\016\001\017\001\255\255\020\001\
\020\001\255\255\022\001\255\255\255\255\025\001\255\255\255\255\
\028\001\030\001\030\001\003\001\004\001\255\255\255\255\255\255\
\037\001\037\001\010\001\011\001\012\001\013\001\014\001\255\255\
\016\001\017\001\255\255\255\255\020\001\255\255\022\001\255\255\
\255\255\025\001\255\255\255\255\028\001\255\255\030\001\020\001\
\010\001\011\001\012\001\013\001\014\001\037\001\016\001\017\001\
\255\255\030\001\020\001\255\255\022\001\255\255\255\255\025\001\
\037\001\255\255\028\001\255\255\030\001\255\255\255\255\255\255\
\255\255\255\255\255\255\037\001\010\001\011\001\012\001\013\001\
\014\001\255\255\016\001\017\001\255\255\255\255\020\001\020\001\
\022\001\020\001\255\255\025\001\255\255\255\255\028\001\255\255\
\030\001\030\001\255\255\030\001\255\255\016\001\017\001\037\001\
\037\001\020\001\037\001\022\001\255\255\255\255\025\001\255\255\
\255\255\028\001\255\255\030\001\255\255\016\001\017\001\255\255\
\255\255\020\001\037\001\022\001\020\001\255\255\025\001\255\255\
\255\255\028\001\255\255\030\001\255\255\255\255\030\001\255\255\
\255\255\255\255\037\001\016\001\017\001\037\001\255\255\020\001\
\255\255\022\001\255\255\255\255\025\001\255\255\255\255\028\001\
\255\255\030\001\255\255\255\255\255\255\016\001\017\001\255\255\
\037\001\020\001\255\255\022\001\255\255\255\255\025\001\255\255\
\255\255\028\001\255\255\030\001\255\255\255\255\255\255\016\001\
\017\001\255\255\037\001\020\001\020\001\022\001\255\255\255\255\
\025\001\255\255\255\255\028\001\255\255\030\001\030\001\255\255\
\255\255\255\255\255\255\255\255\037\001\037\001"

let yynames_const = "\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIV\000\
  MOD\000\
  TRUE\000\
  FALSE\000\
  EQ_TOK\000\
  L_TOK\000\
  LE_TOK\000\
  B_TOK\000\
  BE_TOK\000\
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
  DECLAREBOOL\000\
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
# 78 "parse.mly"
                                             ( Const(_1) )
# 427 "parse.ml"
               : 'aexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 79 "parse.mly"
                                             ( Var _1)
# 434 "parse.ml"
               : 'aexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'aexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aexp) in
    Obj.repr(
# 80 "parse.mly"
                                             ( Add (_1,_3) )
# 442 "parse.ml"
               : 'aexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'aexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aexp) in
    Obj.repr(
# 81 "parse.mly"
                                             ( Sub (_1,_3) )
# 450 "parse.ml"
               : 'aexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'aexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aexp) in
    Obj.repr(
# 82 "parse.mly"
                                             ( Mul (_1,_3) )
# 458 "parse.ml"
               : 'aexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'aexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aexp) in
    Obj.repr(
# 83 "parse.mly"
                                             ( Div (_1,_3) )
# 466 "parse.ml"
               : 'aexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'aexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aexp) in
    Obj.repr(
# 84 "parse.mly"
                                             ( Mod (_1,_3) )
# 474 "parse.ml"
               : 'aexp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'aexp) in
    Obj.repr(
# 85 "parse.mly"
                                             ( _2 )
# 481 "parse.ml"
               : 'aexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'pro_ele) in
    Obj.repr(
# 86 "parse.mly"
                                             ( Proto _1)
# 488 "parse.ml"
               : 'aexp))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : 'exp_inside) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 87 "parse.mly"
                                                                       (  Sizeof1(_3,_5,_7) )
# 497 "parse.ml"
               : 'aexp))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 88 "parse.mly"
                                                     (  Sizeof2(_3,_5) )
# 505 "parse.ml"
               : 'aexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 91 "parse.mly"
                               (Var _1)
# 512 "parse.ml"
               : 'pro_ele))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'exp_inside) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 92 "parse.mly"
                                                                      (     AccessProto1(_1,_3,_5) )
# 521 "parse.ml"
               : 'pro_ele))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'exp_inside) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 93 "parse.mly"
                                                                      (     AccessProto2(_1,_3,_5,_7) )
# 531 "parse.ml"
               : 'pro_ele))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 94 "parse.mly"
                                                     ( AccessProto3(_1,_3) )
# 539 "parse.ml"
               : 'pro_ele))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 95 "parse.mly"
                                                 ( AccessProto4(_1,_3,_5) )
# 548 "parse.ml"
               : 'pro_ele))
; (fun __caml_parser_env ->
    Obj.repr(
# 98 "parse.mly"
                                             ( True )
# 554 "parse.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    Obj.repr(
# 99 "parse.mly"
                                             ( False )
# 560 "parse.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'aexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aexp) in
    Obj.repr(
# 100 "parse.mly"
                                             ( EQ (_1,_3) )
# 568 "parse.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'aexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aexp) in
    Obj.repr(
# 101 "parse.mly"
                                             ( LE(_1,_3) )
# 576 "parse.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'aexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aexp) in
    Obj.repr(
# 102 "parse.mly"
                                             ( L(_1,_3) )
# 584 "parse.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'aexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aexp) in
    Obj.repr(
# 103 "parse.mly"
                                             ( B(_1,_3) )
# 592 "parse.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'aexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aexp) in
    Obj.repr(
# 104 "parse.mly"
                                             ( BE(_1,_3) )
# 600 "parse.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'bexp) in
    Obj.repr(
# 105 "parse.mly"
                                             ( Not(_2) )
# 607 "parse.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'bexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'bexp) in
    Obj.repr(
# 106 "parse.mly"
                                             ( And(_1,_3)  )
# 615 "parse.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'bexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'bexp) in
    Obj.repr(
# 107 "parse.mly"
                                             ( Or(_1,_3) )
# 623 "parse.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'bexp) in
    Obj.repr(
# 108 "parse.mly"
                                             ( _2 )
# 630 "parse.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aexp) in
    Obj.repr(
# 112 "parse.mly"
                                             ( Setint(_1,_3) )
# 638 "parse.ml"
               : Imp.com))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'bexp) in
    Obj.repr(
# 113 "parse.mly"
                                             ( Setbool(_1,_3))
# 646 "parse.ml"
               : Imp.com))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Imp.com) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Imp.com) in
    Obj.repr(
# 114 "parse.mly"
                                             ( Seq(_1,_3) )
# 654 "parse.ml"
               : Imp.com))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'bexp) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Imp.com) in
    Obj.repr(
# 115 "parse.mly"
                              ( If(_2,_4) )
# 662 "parse.ml"
               : Imp.com))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : 'bexp) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : Imp.com) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : Imp.com) in
    Obj.repr(
# 116 "parse.mly"
                                             ( Ifelse(_2,_4,_7) )
# 671 "parse.ml"
               : Imp.com))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'bexp) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Imp.com) in
    Obj.repr(
# 117 "parse.mly"
                                             ( While(_2,_4) )
# 679 "parse.ml"
               : Imp.com))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'aexp) in
    Obj.repr(
# 118 "parse.mly"
                                             ( Print(_2) )
# 686 "parse.ml"
               : Imp.com))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Imp.com) in
    Obj.repr(
# 119 "parse.mly"
                                             ( _2 )
# 693 "parse.ml"
               : Imp.com))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 121 "parse.mly"
                               ( Declareint(_2) )
# 700 "parse.ml"
               : Imp.com))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 122 "parse.mly"
                                             ( Declarebool(_2) )
# 707 "parse.ml"
               : Imp.com))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 8 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _9 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 123 "parse.mly"
                                                                                          ( Declareproto(_2,_4,_9) )
# 717 "parse.ml"
               : Imp.com))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 124 "parse.mly"
                                                ( Readfrom(_1,_4) )
# 725 "parse.ml"
               : Imp.com))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 125 "parse.mly"
                                                ( Writeto(_1,_4) )
# 733 "parse.ml"
               : Imp.com))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'exp_inside) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'aexp) in
    Obj.repr(
# 127 "parse.mly"
                                                                                   ( SetProto1(_1,_3,_5,_7) )
# 743 "parse.ml"
               : Imp.com))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 9 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 7 : 'exp_inside) in
    let _5 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _7 = (Parsing.peek_val __caml_parser_env 3 : int) in
    let _10 = (Parsing.peek_val __caml_parser_env 0 : 'aexp) in
    Obj.repr(
# 128 "parse.mly"
                                                                               ( SetProto2(_1,_3,_5,_7,_10) )
# 754 "parse.ml"
               : Imp.com))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'aexp) in
    Obj.repr(
# 129 "parse.mly"
                                                                        (     SetProto3(_1,_3,_5) )
# 763 "parse.ml"
               : Imp.com))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : int) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'aexp) in
    Obj.repr(
# 130 "parse.mly"
                                                                        (     SetProto4(_1,_3,_5,_8) )
# 773 "parse.ml"
               : Imp.com))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'aexp) in
    Obj.repr(
# 132 "parse.mly"
                                                       (AddEle1(_1,_3,_7))
# 782 "parse.ml"
               : Imp.com))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 8 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'exp_inside) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'aexp) in
    Obj.repr(
# 133 "parse.mly"
                                                                         (AddEle2(_1,_3,_5,_9))
# 792 "parse.ml"
               : Imp.com))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string) in
    Obj.repr(
# 134 "parse.mly"
                                                  (AddEle3(_1,_3))
# 800 "parse.ml"
               : Imp.com))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'exp_inside) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : string) in
    Obj.repr(
# 135 "parse.mly"
                                                                     (AddEle4(_1,_3,_5))
# 809 "parse.ml"
               : Imp.com))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'pro_ele) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'pro_ele) in
    Obj.repr(
# 136 "parse.mly"
                           ( Copyfrom(_1,_3) )
# 817 "parse.ml"
               : Imp.com))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 141 "parse.mly"
              ( ExpEle1 _1 )
# 824 "parse.ml"
               : 'exp_inside))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 142 "parse.mly"
                                ( ExpEle2 (_1, _3) )
# 832 "parse.ml"
               : 'exp_inside))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp_inside) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp_inside) in
    Obj.repr(
# 143 "parse.mly"
                                       ( ExpEle3 (_1, _3) )
# 840 "parse.ml"
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
