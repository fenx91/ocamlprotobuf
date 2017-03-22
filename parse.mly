%{
(* 
 * Parser for PF language. 
 *)

open Pf		  

let error msg	= failwith msg

%}

%token <string>         IDENTIFIER
%token <string>         INT

%token PLUS 
%token MINUS 
%token TIMES 
%token DIV 
%token MOD 
%token TRUE
%token FALSE
%token EQ_TOK
%token L_TOK
%token LE_TOK
%token B_TOK
%token BE_TOK
%token NOT
%token AND
%token OR 
%token SKIP
%token SET 
%token SEMICOLON
%token IF
%token THEN
%token ELSE
%token WHILE
%token DO 
%token PRINT
%token PPRINT
%token LPAREN
%token COPYFROM
%token RPAREN
%token LBRACE
%token RBRACE
%token LBRACK
%token RBRACK
%token QUOTE
%token DECLAREINT
%token DECLAREPROTO
%token DECLAREBOOL
%token DECLARESTR
%token END
%token PERIOD
%token DOLLAR
%token QUESTION
%token READFROM
%token WRITETO
%token SIZEOF
%token EOF

%start com
%type <Pf.com> com

%right SET
%right LBRACK
%right NOT

%left AND
%left OR
%left PLUS MINUS
%left TIMES
%left L_TOK LE_TOK B_TOK BE_TOK EQ_TOK
%left DOLLAR
%left RBRACK

%nonassoc ELSE
%left COPYFROM
%%

index : INT					{IndexConst($1)}
| IDENTIFIER					{IndexIden($1)}
;

exp : INT                                   { Const($1) }
| IDENTIFIER                                 { Var $1}
| exp PLUS exp                             { Add ($1,$3) } 
| exp MINUS exp                            { Sub ($1,$3) } 
| exp TIMES exp                            { Mul ($1,$3) } 
| exp DIV exp                              { Div ($1,$3) } 
| exp MOD exp                              { Mod ($1,$3) } 
| pro_ele                                    { Protoele $1} 
| SIZEOF LPAREN IDENTIFIER DOLLAR exp_inside DOLLAR IDENTIFIER RPAREN  {  Sizeof1($3,$5,$7) }
| SIZEOF LPAREN IDENTIFIER DOLLAR IDENTIFIER RPAREN  {  Sizeof2($3,$5) }
| TRUE                                  { True }
| FALSE                                      { False }
| exp EQ_TOK exp                           { EQ ($1,$3) }
| exp LE_TOK exp                           { LE($1,$3) }
| exp L_TOK exp                            { L($1,$3) }
| exp B_TOK exp                            { B($1,$3) }
| exp BE_TOK exp                           { BE($1,$3) }
| NOT exp                                   { Not($2) }
| exp AND exp                              { And($1,$3)  }
| exp OR exp                               { Or($1,$3) }
| LPAREN exp RPAREN                         { $2 }
| QUOTE IDENTIFIER QUOTE                     { Str($2) } 
| PPRINT IDENTIFIER                          { PPrint($2) }

| IDENTIFIER DOLLAR exp_inside DOLLAR IDENTIFIER QUESTION     { HasProto1($1,$3,$5) }
| IDENTIFIER DOLLAR IDENTIFIER QUESTION                       { HasProto2($1,$3) }
 
;         

pro_ele:   
| IDENTIFIER DOLLAR exp_inside DOLLAR IDENTIFIER                      {     AccessProto1($1,$3,$5) }
| IDENTIFIER DOLLAR exp_inside DOLLAR IDENTIFIER LBRACK index RBRACK    {     AccessProto2($1,$3,$5,$7) }
| IDENTIFIER DOLLAR IDENTIFIER                       { AccessProto3($1,$3) }
| IDENTIFIER DOLLAR IDENTIFIER LBRACK index RBRACK { AccessProto4($1,$3,$5) }
;

com :
| IDENTIFIER SET exp                         { Set($1,$3) }
 
| com SEMICOLON com                          { Seq($1,$3) }
| IF exp THEN com END			     { If($2,$4) }
| IF exp THEN com ELSE com END           { Ifelse($2,$4,$6) }
| WHILE exp DO com END                       { While($2,$4) }
| PRINT exp                           { Print($2) }
| LPAREN com RPAREN                          { $2 } 

| DECLAREINT IDENTIFIER			     { Declareint($2) }
| DECLAREBOOL IDENTIFIER                     { Declarebool($2) }
| DECLARESTR IDENTIFIER                      { Declarestr($2) }
| DECLAREPROTO IDENTIFIER QUOTE IDENTIFIER PERIOD IDENTIFIER QUOTE QUOTE IDENTIFIER QUOTE { Declareproto($2,$4,$6,$9) } 

| IDENTIFIER READFROM QUOTE IDENTIFIER QUOTE INT  { Readfrom1($1,$4,$6) }  
| IDENTIFIER READFROM QUOTE IDENTIFIER QUOTE    { Readfrom($1,$4) } 
| IDENTIFIER WRITETO QUOTE IDENTIFIER QUOTE INT    { Writeto1($1,$4,$6) }
| IDENTIFIER WRITETO QUOTE IDENTIFIER QUOTE     { Writeto($1,$4) } 

| IDENTIFIER DOLLAR exp_inside DOLLAR IDENTIFIER SET exp                          { SetProto1($1,$3,$5,$7) }
| IDENTIFIER DOLLAR exp_inside DOLLAR IDENTIFIER LBRACK index RBRACK SET exp    { SetProto2($1,$3,$5,$7,$10) }
| IDENTIFIER DOLLAR IDENTIFIER SET exp                                 { SetProto3($1,$3,$5) }
| IDENTIFIER DOLLAR IDENTIFIER LBRACK index RBRACK SET exp               { SetProto4($1,$3,$5,$8) }

| IDENTIFIER DOLLAR IDENTIFIER LBRACK PLUS RBRACK exp {AddEle1($1,$3,$7)}
| IDENTIFIER DOLLAR exp_inside DOLLAR IDENTIFIER LBRACK PLUS RBRACK exp {AddEle2($1,$3,$5,$9)}
| IDENTIFIER DOLLAR IDENTIFIER LBRACK PLUS RBRACK {AddEle3($1,$3)}
| IDENTIFIER DOLLAR exp_inside DOLLAR IDENTIFIER LBRACK PLUS RBRACK  {AddEle4($1,$3,$5)}
| exp COPYFROM IDENTIFIER {Copyfrom($1, $3)}
;


exp_inside :
  IDENTIFIER  { ExpEle1 $1 }
| IDENTIFIER LBRACK index RBRACK  { ExpEle2 ($1, $3) } 
| exp_inside DOLLAR exp_inside         { ExpEle3 ($1, $3) };
