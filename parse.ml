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

# 65 "parse.ml"
let yytransl_const = [|
  260 (* PLUS *);
  261 (* MINUS *);
  262 (* TIMES *);
  263 (* DIV *);
  264 (* MOD *);
  265 (* TRUE *);
  266 (* FALSE *);
  267 (* EQ_TOK *);
  268 (* L_TOK *);
  269 (* LE_TOK *);
  270 (* B_TOK *);
  271 (* BE_TOK *);
  272 (* NOT *);
  273 (* AND *);
  274 (* OR *);
  275 (* SKIP *);
  276 (* SET *);
  277 (* SEMICOLON *);
  278 (* IF *);
  279 (* THEN *);
  280 (* ELSE *);
  281 (* WHILE *);
  282 (* DO *);
  283 (* PRINT *);
  284 (* PPRINT *);
  285 (* LPAREN *);
  286 (* RPAREN *);
  287 (* LBRACE *);
  288 (* RBRACE *);
  289 (* LBRACK *);
  290 (* RBRACK *);
  291 (* QUOTE *);
  292 (* DECLAREINT *);
  293 (* DECLAREPROTO *);
  294 (* DECLAREBOOL *);
  295 (* DECLARESTR *);
  296 (* END *);
  297 (* PERIOD *);
  298 (* DOLLAR *);
  299 (* READFROM *);
  300 (* WRITETO *);
  301 (* COPYFROM *);
  302 (* SIZEOF *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* IDENTIFIER *);
  258 (* INT *);
  259 (* STRING *);
    0|]

let yylhs = "\255\255\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\003\000\003\000\003\000\003\000\003\000\
\005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
\005\000\005\000\005\000\006\000\006\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\004\000\004\000\004\000\
\000\000"

let yylen = "\002\000\
\001\000\001\000\003\000\003\000\003\000\003\000\003\000\003\000\
\001\000\008\000\006\000\001\000\005\000\008\000\003\000\006\000\
\001\000\001\000\003\000\003\000\003\000\003\000\003\000\002\000\
\003\000\003\000\003\000\001\000\002\000\003\000\003\000\003\000\
\003\000\005\000\008\000\004\000\002\000\003\000\002\000\002\000\
\002\000\010\000\005\000\005\000\007\000\010\000\005\000\008\000\
\007\000\009\000\006\000\008\000\003\000\001\000\004\000\003\000\
\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\001\000\017\000\018\000\000\000\000\000\000\000\
\000\000\009\000\000\000\000\000\037\000\000\000\039\000\000\000\
\040\000\041\000\000\000\000\000\028\000\000\000\000\000\000\000\
\032\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\038\000\000\000\000\000\000\000\053\000\029\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\008\000\027\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\026\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\056\000\043\000\044\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\011\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\035\000\000\000\
\000\000\000\000\055\000\010\000\000\000\000\000\042\000"

let yydgoto = "\002\000\
\012\000\025\000\026\000\102\000\027\000\041\000"

let yysindex = "\018\000\
\135\255\000\000\239\254\112\255\112\255\040\255\135\255\060\255\
\096\255\097\255\129\255\045\255\033\255\124\255\131\255\041\255\
\103\255\095\255\000\000\000\000\000\000\112\255\112\255\113\255\
\191\255\000\000\077\255\070\255\000\000\054\255\000\000\108\255\
\000\000\000\000\135\255\144\255\000\000\147\255\191\255\039\255\
\000\000\244\254\107\255\149\255\153\255\154\255\039\255\171\255\
\248\254\155\255\031\255\031\255\031\255\031\255\031\255\031\255\
\031\255\031\255\031\255\031\255\112\255\112\255\135\255\135\255\
\000\000\158\255\045\255\095\255\000\000\000\000\031\255\089\255\
\160\255\130\255\132\255\136\255\126\255\000\000\000\000\138\255\
\031\255\104\255\104\255\006\255\203\255\203\255\203\255\203\255\
\203\255\203\255\203\255\174\255\000\000\247\254\045\255\152\255\
\203\255\166\255\178\255\065\255\004\255\000\000\000\000\000\000\
\212\255\214\255\216\255\183\255\192\255\223\255\000\000\031\255\
\205\255\031\255\100\255\225\255\166\255\194\255\241\254\186\255\
\135\255\196\255\203\255\031\255\203\255\195\255\198\255\075\255\
\200\255\232\255\000\000\233\255\235\255\252\254\202\255\203\255\
\000\000\031\255\218\255\195\255\206\255\059\255\000\000\238\255\
\203\255\031\255\000\000\000\000\208\255\203\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\199\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\241\000\000\000\000\000\000\000\000\000\
\000\000\163\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\004\000\005\000\
\000\000\061\255\000\000\000\000\000\000\000\000\123\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\039\000\038\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\085\000\000\000\000\000\000\000\000\000\
\000\000\218\000\242\000\194\000\010\001\034\001\052\001\068\001\
\084\001\104\001\120\001\084\000\000\000\000\000\040\000\000\000\
\042\000\215\255\000\000\000\000\102\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\055\000\000\000\001\000\119\000\
\000\000\000\000\000\000\000\000\000\000\124\000\204\255\000\000\
\000\000\000\000\151\000\000\000\164\000\215\255\000\000\000\000\
\204\255\000\000\000\000\000\000\000\000\000\000\000\000\181\000\
\043\000\230\000\000\000\000\000\000\000\204\255\000\000\000\000\
\254\000\000\000\000\000\000\000\000\000\040\001\000\000"

let yygindex = "\000\000\
\251\255\249\255\255\255\008\000\006\000\000\000"

let yytablesize = 672
let yytable = "\013\000\
\016\000\030\000\014\000\030\000\031\000\013\000\039\000\071\000\
\061\000\062\000\028\000\035\000\054\000\055\000\131\000\048\000\
\035\000\132\000\001\000\040\000\072\000\079\000\043\000\114\000\
\015\000\016\000\017\000\047\000\049\000\067\000\109\000\018\000\
\019\000\013\000\069\000\143\000\115\000\012\000\033\000\036\000\
\029\000\047\000\014\000\082\000\083\000\084\000\085\000\086\000\
\087\000\088\000\089\000\090\000\091\000\077\000\034\000\061\000\
\062\000\094\000\095\000\081\000\031\000\013\000\013\000\097\000\
\100\000\035\000\092\000\093\000\051\000\052\000\053\000\054\000\
\055\000\108\000\035\000\044\000\024\000\036\000\051\000\052\000\
\053\000\054\000\055\000\025\000\015\000\065\000\061\000\062\000\
\148\000\018\000\098\000\132\000\099\000\061\000\062\000\064\000\
\032\000\033\000\113\000\063\000\018\000\126\000\054\000\127\000\
\123\000\015\000\125\000\128\000\139\000\053\000\054\000\055\000\
\018\000\019\000\120\000\134\000\136\000\081\000\051\000\013\000\
\020\000\021\000\024\000\013\000\018\000\019\000\037\000\022\000\
\081\000\034\000\145\000\042\000\020\000\021\000\024\000\003\000\
\046\000\045\000\150\000\022\000\023\000\050\000\066\000\054\000\
\068\000\024\000\013\000\070\000\073\000\074\000\049\000\038\000\
\023\000\075\000\076\000\080\000\004\000\024\000\096\000\005\000\
\101\000\006\000\002\000\045\000\103\000\007\000\104\000\106\000\
\105\000\024\000\008\000\009\000\010\000\011\000\051\000\052\000\
\053\000\054\000\055\000\107\000\048\000\056\000\057\000\058\000\
\059\000\060\000\051\000\052\000\053\000\054\000\055\000\062\000\
\110\000\005\000\051\000\052\000\053\000\054\000\055\000\111\000\
\078\000\056\000\057\000\058\000\059\000\060\000\051\000\052\000\
\053\000\054\000\055\000\112\000\078\000\117\000\118\000\121\000\
\119\000\003\000\001\000\001\000\001\000\001\000\001\000\122\000\
\124\000\129\000\130\000\133\000\137\000\052\000\135\000\138\000\
\132\000\140\000\141\000\142\000\144\000\146\000\149\000\147\000\
\057\000\004\000\151\000\012\000\000\000\054\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\050\000\000\000\000\000\
\000\000\000\000\000\000\000\000\016\000\016\000\016\000\016\000\
\016\000\006\000\000\000\016\000\016\000\016\000\016\000\016\000\
\000\000\016\000\016\000\000\000\000\000\016\000\000\000\016\000\
\030\000\031\000\016\000\000\000\000\000\000\000\016\000\000\000\
\016\000\007\000\016\000\030\000\031\000\000\000\000\000\046\000\
\016\000\000\000\055\000\030\000\031\000\016\000\014\000\014\000\
\014\000\014\000\014\000\019\000\000\000\014\000\014\000\014\000\
\014\000\014\000\012\000\014\000\014\000\000\000\047\000\014\000\
\000\000\014\000\000\000\021\000\014\000\012\000\033\000\036\000\
\014\000\047\000\014\000\034\000\014\000\012\000\033\000\036\000\
\000\000\047\000\014\000\020\000\055\000\000\000\034\000\014\000\
\015\000\015\000\015\000\015\000\015\000\000\000\034\000\015\000\
\015\000\015\000\015\000\015\000\025\000\015\000\015\000\022\000\
\025\000\015\000\025\000\015\000\000\000\025\000\015\000\000\000\
\000\000\025\000\015\000\025\000\015\000\000\000\015\000\023\000\
\000\000\000\000\000\000\025\000\015\000\000\000\054\000\013\000\
\013\000\013\000\013\000\013\000\000\000\000\000\013\000\013\000\
\013\000\013\000\013\000\051\000\013\000\013\000\000\000\024\000\
\013\000\024\000\013\000\000\000\024\000\013\000\051\000\000\000\
\024\000\013\000\024\000\013\000\000\000\013\000\051\000\000\000\
\000\000\000\000\024\000\013\000\000\000\054\000\002\000\002\000\
\002\000\002\000\002\000\049\000\000\000\002\000\002\000\002\000\
\002\000\002\000\000\000\002\000\002\000\000\000\049\000\002\000\
\045\000\002\000\000\000\000\000\002\000\000\000\049\000\000\000\
\002\000\000\000\002\000\045\000\002\000\005\000\005\000\005\000\
\000\000\048\000\002\000\045\000\005\000\005\000\005\000\005\000\
\005\000\000\000\005\000\005\000\048\000\000\000\005\000\000\000\
\005\000\000\000\000\000\005\000\048\000\003\000\003\000\005\000\
\000\000\005\000\000\000\005\000\003\000\003\000\003\000\003\000\
\003\000\005\000\003\000\003\000\000\000\000\000\003\000\000\000\
\003\000\000\000\000\000\003\000\000\000\004\000\004\000\003\000\
\000\000\003\000\052\000\003\000\004\000\004\000\004\000\004\000\
\004\000\003\000\004\000\004\000\000\000\052\000\004\000\000\000\
\004\000\000\000\000\000\004\000\000\000\052\000\000\000\004\000\
\000\000\004\000\050\000\004\000\006\000\006\000\006\000\006\000\
\006\000\004\000\006\000\006\000\000\000\050\000\006\000\000\000\
\006\000\000\000\000\000\006\000\000\000\050\000\000\000\006\000\
\000\000\006\000\000\000\006\000\007\000\007\000\007\000\007\000\
\007\000\006\000\007\000\007\000\000\000\000\000\007\000\000\000\
\007\000\000\000\000\000\007\000\046\000\000\000\000\000\007\000\
\000\000\007\000\000\000\007\000\019\000\019\000\000\000\046\000\
\019\000\007\000\019\000\000\000\000\000\019\000\000\000\046\000\
\000\000\019\000\000\000\019\000\021\000\021\000\000\000\000\000\
\021\000\000\000\021\000\019\000\000\000\021\000\000\000\000\000\
\000\000\021\000\000\000\021\000\020\000\020\000\000\000\000\000\
\020\000\000\000\020\000\021\000\000\000\020\000\000\000\000\000\
\000\000\020\000\000\000\020\000\000\000\000\000\000\000\000\000\
\022\000\022\000\000\000\020\000\022\000\000\000\022\000\000\000\
\000\000\022\000\000\000\000\000\000\000\022\000\000\000\022\000\
\023\000\023\000\000\000\000\000\023\000\000\000\023\000\022\000\
\000\000\023\000\000\000\000\000\000\000\023\000\000\000\023\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\023\000"

let yycheck = "\001\000\
\000\000\007\000\020\001\000\000\000\000\007\000\014\000\020\001\
\017\001\018\001\005\000\021\001\007\001\008\001\030\001\023\000\
\021\001\033\001\001\000\014\000\033\001\030\001\015\000\020\001\
\042\001\043\001\044\001\022\000\023\000\035\000\040\001\001\001\
\002\001\035\000\036\000\040\001\033\001\000\000\000\000\000\000\
\001\001\000\000\000\000\051\000\052\000\053\000\054\000\055\000\
\056\000\057\000\058\000\059\000\060\000\046\000\000\000\017\001\
\018\001\063\000\064\000\029\001\001\001\063\000\064\000\071\000\
\072\000\021\001\061\000\062\000\004\001\005\001\006\001\007\001\
\008\001\081\000\021\001\035\001\046\001\045\001\004\001\005\001\
\006\001\007\001\008\001\000\000\000\000\032\001\017\001\018\001\
\030\001\001\001\002\001\033\001\004\001\017\001\018\001\026\001\
\001\001\001\001\034\001\023\001\001\001\002\001\042\001\004\001\
\112\000\045\001\114\000\115\000\034\001\006\001\007\001\008\001\
\001\001\002\001\107\000\121\000\124\000\029\001\000\000\121\000\
\009\001\010\001\000\000\000\000\001\001\002\001\003\001\016\001\
\029\001\001\001\138\000\001\001\009\001\010\001\046\001\001\001\
\042\001\035\001\146\000\016\001\029\001\029\001\035\001\042\001\
\001\001\046\001\045\001\001\001\042\001\001\001\000\000\028\001\
\029\001\001\001\001\001\001\001\022\001\046\001\001\001\025\001\
\001\001\027\001\000\000\000\000\035\001\031\001\035\001\042\001\
\033\001\046\001\036\001\037\001\038\001\039\001\004\001\005\001\
\006\001\007\001\008\001\042\001\000\000\011\001\012\001\013\001\
\014\001\015\001\004\001\005\001\006\001\007\001\008\001\018\001\
\041\001\000\000\004\001\005\001\006\001\007\001\008\001\034\001\
\030\001\011\001\012\001\013\001\014\001\015\001\004\001\005\001\
\006\001\007\001\008\001\034\001\030\001\002\001\001\001\024\001\
\001\001\000\000\004\001\005\001\006\001\007\001\008\001\001\001\
\020\001\001\001\033\001\042\001\034\001\000\000\035\001\034\001\
\033\001\002\001\002\001\001\001\035\001\020\001\001\001\034\001\
\000\000\000\000\035\001\045\001\255\255\042\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\000\000\255\255\255\255\
\255\255\255\255\255\255\255\255\004\001\005\001\006\001\007\001\
\008\001\000\000\255\255\011\001\012\001\013\001\014\001\015\001\
\255\255\017\001\018\001\255\255\255\255\021\001\255\255\023\001\
\021\001\021\001\026\001\255\255\255\255\255\255\030\001\255\255\
\032\001\000\000\034\001\032\001\032\001\255\255\255\255\000\000\
\040\001\255\255\042\001\040\001\040\001\045\001\004\001\005\001\
\006\001\007\001\008\001\000\000\255\255\011\001\012\001\013\001\
\014\001\015\001\021\001\017\001\018\001\255\255\021\001\021\001\
\255\255\023\001\255\255\000\000\026\001\032\001\032\001\032\001\
\030\001\032\001\032\001\021\001\034\001\040\001\040\001\040\001\
\255\255\040\001\040\001\000\000\042\001\255\255\032\001\045\001\
\004\001\005\001\006\001\007\001\008\001\255\255\040\001\011\001\
\012\001\013\001\014\001\015\001\017\001\017\001\018\001\000\000\
\021\001\021\001\023\001\023\001\255\255\026\001\026\001\255\255\
\255\255\030\001\030\001\032\001\032\001\255\255\034\001\000\000\
\255\255\255\255\255\255\040\001\040\001\255\255\042\001\004\001\
\005\001\006\001\007\001\008\001\255\255\255\255\011\001\012\001\
\013\001\014\001\015\001\021\001\017\001\018\001\255\255\021\001\
\021\001\023\001\023\001\255\255\026\001\026\001\032\001\255\255\
\030\001\030\001\032\001\032\001\255\255\034\001\040\001\255\255\
\255\255\255\255\040\001\040\001\255\255\042\001\004\001\005\001\
\006\001\007\001\008\001\021\001\255\255\011\001\012\001\013\001\
\014\001\015\001\255\255\017\001\018\001\255\255\032\001\021\001\
\021\001\023\001\255\255\255\255\026\001\255\255\040\001\255\255\
\030\001\255\255\032\001\032\001\034\001\004\001\005\001\006\001\
\255\255\021\001\040\001\040\001\011\001\012\001\013\001\014\001\
\015\001\255\255\017\001\018\001\032\001\255\255\021\001\255\255\
\023\001\255\255\255\255\026\001\040\001\004\001\005\001\030\001\
\255\255\032\001\255\255\034\001\011\001\012\001\013\001\014\001\
\015\001\040\001\017\001\018\001\255\255\255\255\021\001\255\255\
\023\001\255\255\255\255\026\001\255\255\004\001\005\001\030\001\
\255\255\032\001\021\001\034\001\011\001\012\001\013\001\014\001\
\015\001\040\001\017\001\018\001\255\255\032\001\021\001\255\255\
\023\001\255\255\255\255\026\001\255\255\040\001\255\255\030\001\
\255\255\032\001\021\001\034\001\011\001\012\001\013\001\014\001\
\015\001\040\001\017\001\018\001\255\255\032\001\021\001\255\255\
\023\001\255\255\255\255\026\001\255\255\040\001\255\255\030\001\
\255\255\032\001\255\255\034\001\011\001\012\001\013\001\014\001\
\015\001\040\001\017\001\018\001\255\255\255\255\021\001\255\255\
\023\001\255\255\255\255\026\001\021\001\255\255\255\255\030\001\
\255\255\032\001\255\255\034\001\017\001\018\001\255\255\032\001\
\021\001\040\001\023\001\255\255\255\255\026\001\255\255\040\001\
\255\255\030\001\255\255\032\001\017\001\018\001\255\255\255\255\
\021\001\255\255\023\001\040\001\255\255\026\001\255\255\255\255\
\255\255\030\001\255\255\032\001\017\001\018\001\255\255\255\255\
\021\001\255\255\023\001\040\001\255\255\026\001\255\255\255\255\
\255\255\030\001\255\255\032\001\255\255\255\255\255\255\255\255\
\017\001\018\001\255\255\040\001\021\001\255\255\023\001\255\255\
\255\255\026\001\255\255\255\255\255\255\030\001\255\255\032\001\
\017\001\018\001\255\255\255\255\021\001\255\255\023\001\040\001\
\255\255\026\001\255\255\255\255\255\255\030\001\255\255\032\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\040\001"

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
  PPRINT\000\
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
  DECLARESTR\000\
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
  STRING\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 81 "parse.mly"
                                             ( Const(_1) )
# 441 "parse.ml"
               : 'aexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 82 "parse.mly"
                                             ( Var _1)
# 448 "parse.ml"
               : 'aexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'aexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aexp) in
    Obj.repr(
# 83 "parse.mly"
                                             ( Add (_1,_3) )
# 456 "parse.ml"
               : 'aexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'aexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aexp) in
    Obj.repr(
# 84 "parse.mly"
                                             ( Sub (_1,_3) )
# 464 "parse.ml"
               : 'aexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'aexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aexp) in
    Obj.repr(
# 85 "parse.mly"
                                             ( Mul (_1,_3) )
# 472 "parse.ml"
               : 'aexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'aexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aexp) in
    Obj.repr(
# 86 "parse.mly"
                                             ( Div (_1,_3) )
# 480 "parse.ml"
               : 'aexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'aexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aexp) in
    Obj.repr(
# 87 "parse.mly"
                                             ( Mod (_1,_3) )
# 488 "parse.ml"
               : 'aexp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'aexp) in
    Obj.repr(
# 88 "parse.mly"
                                             ( _2 )
# 495 "parse.ml"
               : 'aexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'pro_ele) in
    Obj.repr(
# 89 "parse.mly"
                                             ( Proto _1)
# 502 "parse.ml"
               : 'aexp))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : 'exp_inside) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 90 "parse.mly"
                                                                       (  Sizeof1(_3,_5,_7) )
# 511 "parse.ml"
               : 'aexp))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 91 "parse.mly"
                                                     (  Sizeof2(_3,_5) )
# 519 "parse.ml"
               : 'aexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 94 "parse.mly"
                               (Var _1)
# 526 "parse.ml"
               : 'pro_ele))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'exp_inside) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 95 "parse.mly"
                                                                      (     AccessProto1(_1,_3,_5) )
# 535 "parse.ml"
               : 'pro_ele))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'exp_inside) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 96 "parse.mly"
                                                                      (     AccessProto2(_1,_3,_5,_7) )
# 545 "parse.ml"
               : 'pro_ele))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 97 "parse.mly"
                                                     ( AccessProto3(_1,_3) )
# 553 "parse.ml"
               : 'pro_ele))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 98 "parse.mly"
                                                 ( AccessProto4(_1,_3,_5) )
# 562 "parse.ml"
               : 'pro_ele))
; (fun __caml_parser_env ->
    Obj.repr(
# 101 "parse.mly"
                                             ( True )
# 568 "parse.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    Obj.repr(
# 102 "parse.mly"
                                             ( False )
# 574 "parse.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'aexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aexp) in
    Obj.repr(
# 103 "parse.mly"
                                             ( EQ (_1,_3) )
# 582 "parse.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'aexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aexp) in
    Obj.repr(
# 104 "parse.mly"
                                             ( LE(_1,_3) )
# 590 "parse.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'aexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aexp) in
    Obj.repr(
# 105 "parse.mly"
                                             ( L(_1,_3) )
# 598 "parse.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'aexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aexp) in
    Obj.repr(
# 106 "parse.mly"
                                             ( B(_1,_3) )
# 606 "parse.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'aexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aexp) in
    Obj.repr(
# 107 "parse.mly"
                                             ( BE(_1,_3) )
# 614 "parse.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'bexp) in
    Obj.repr(
# 108 "parse.mly"
                                             ( Not(_2) )
# 621 "parse.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'bexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'bexp) in
    Obj.repr(
# 109 "parse.mly"
                                             ( And(_1,_3)  )
# 629 "parse.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'bexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'bexp) in
    Obj.repr(
# 110 "parse.mly"
                                             ( Or(_1,_3) )
# 637 "parse.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'bexp) in
    Obj.repr(
# 111 "parse.mly"
                                             ( _2 )
# 644 "parse.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 114 "parse.mly"
                                             ( Str(_1) )
# 651 "parse.ml"
               : 'strexp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 115 "parse.mly"
                                             ( PPrint(_2) )
# 658 "parse.ml"
               : 'strexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aexp) in
    Obj.repr(
# 118 "parse.mly"
                                             ( Setint(_1,_3) )
# 666 "parse.ml"
               : Imp.com))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'bexp) in
    Obj.repr(
# 119 "parse.mly"
                                             ( Setbool(_1,_3) )
# 674 "parse.ml"
               : Imp.com))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'strexp) in
    Obj.repr(
# 120 "parse.mly"
                                             ( Setstr (_1,_3) )
# 682 "parse.ml"
               : Imp.com))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Imp.com) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Imp.com) in
    Obj.repr(
# 122 "parse.mly"
                                             ( Seq(_1,_3) )
# 690 "parse.ml"
               : Imp.com))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'bexp) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Imp.com) in
    Obj.repr(
# 123 "parse.mly"
                              ( If(_2,_4) )
# 698 "parse.ml"
               : Imp.com))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : 'bexp) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : Imp.com) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : Imp.com) in
    Obj.repr(
# 124 "parse.mly"
                                             ( Ifelse(_2,_4,_7) )
# 707 "parse.ml"
               : Imp.com))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'bexp) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Imp.com) in
    Obj.repr(
# 125 "parse.mly"
                                             ( While(_2,_4) )
# 715 "parse.ml"
               : Imp.com))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 126 "parse.mly"
                                             ( Print(_2) )
# 722 "parse.ml"
               : Imp.com))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Imp.com) in
    Obj.repr(
# 127 "parse.mly"
                                             ( _2 )
# 729 "parse.ml"
               : Imp.com))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 129 "parse.mly"
                               ( Declareint(_2) )
# 736 "parse.ml"
               : Imp.com))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 130 "parse.mly"
                                             ( Declarebool(_2) )
# 743 "parse.ml"
               : Imp.com))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 131 "parse.mly"
                                             ( Declarestr(_2) )
# 750 "parse.ml"
               : Imp.com))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 8 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _9 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 132 "parse.mly"
                                                                                          ( Declareproto(_2,_4,_9) )
# 760 "parse.ml"
               : Imp.com))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 133 "parse.mly"
                                                ( Readfrom(_1,_4) )
# 768 "parse.ml"
               : Imp.com))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 134 "parse.mly"
                                                ( Writeto(_1,_4) )
# 776 "parse.ml"
               : Imp.com))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'exp_inside) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'aexp) in
    Obj.repr(
# 136 "parse.mly"
                                                                                   ( SetProto1(_1,_3,_5,_7) )
# 786 "parse.ml"
               : Imp.com))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 9 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 7 : 'exp_inside) in
    let _5 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _7 = (Parsing.peek_val __caml_parser_env 3 : 'aexp) in
    let _10 = (Parsing.peek_val __caml_parser_env 0 : 'aexp) in
    Obj.repr(
# 137 "parse.mly"
                                                                                ( SetProto2(_1,_3,_5,_7,_10) )
# 797 "parse.ml"
               : Imp.com))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'aexp) in
    Obj.repr(
# 138 "parse.mly"
                                                                        (     SetProto3(_1,_3,_5) )
# 806 "parse.ml"
               : Imp.com))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : 'aexp) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'aexp) in
    Obj.repr(
# 139 "parse.mly"
                                                                         (     SetProto4(_1,_3,_5,_8) )
# 816 "parse.ml"
               : Imp.com))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'aexp) in
    Obj.repr(
# 141 "parse.mly"
                                                       (AddEle1(_1,_3,_7))
# 825 "parse.ml"
               : Imp.com))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 8 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'exp_inside) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'aexp) in
    Obj.repr(
# 142 "parse.mly"
                                                                         (AddEle2(_1,_3,_5,_9))
# 835 "parse.ml"
               : Imp.com))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string) in
    Obj.repr(
# 143 "parse.mly"
                                                  (AddEle3(_1,_3))
# 843 "parse.ml"
               : Imp.com))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'exp_inside) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : string) in
    Obj.repr(
# 144 "parse.mly"
                                                                     (AddEle4(_1,_3,_5))
# 852 "parse.ml"
               : Imp.com))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'pro_ele) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'pro_ele) in
    Obj.repr(
# 145 "parse.mly"
                           ( Copyfrom(_1,_3) )
# 860 "parse.ml"
               : Imp.com))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 150 "parse.mly"
              ( ExpEle1 _1 )
# 867 "parse.ml"
               : 'exp_inside))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 151 "parse.mly"
                                ( ExpEle2 (_1, _3) )
# 875 "parse.ml"
               : 'exp_inside))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp_inside) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp_inside) in
    Obj.repr(
# 152 "parse.mly"
                                       ( ExpEle3 (_1, _3) )
# 883 "parse.ml"
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
