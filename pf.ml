type loc = string

type index =
  | IndexConst of string
  | IndexIden of string

type exp_inside =
  | ExpEle1 of loc
  | ExpEle2 of loc * index
  | ExpEle3 of exp_inside * exp_inside

type pro_ele =
  | AccessProto1 of loc * exp_inside * loc
  | AccessProto2 of loc * exp_inside * loc * index
  | AccessProto3 of loc * loc
  | AccessProto4 of loc * loc * index

type exp =   
(*arithm exp*)
  | Const of string                (* n *)
  | Var of loc
  | Add of exp * exp          (* a0 + a1 *) 
  | Sub of exp * exp          (* a0 - a1 *) 
  | Mul of exp * exp          (* a0 * a1 *) 
  | Div of exp * exp          (* a0 / a1 *) 
  | Mod of exp * exp          (* a0 % a1 *) 

  | Protoele of pro_ele

  | Sizeof1 of loc * exp_inside * loc
  | Sizeof2 of loc * loc

(*bool exp*)
  | True
  | False
  | EQ of exp * exp           (* a0 == a1 *) 
  | LE of exp * exp           (* a0 <= a2 *) 
  | BE of exp * exp
  | L  of exp * exp           (* a0 < a1 *) 
  | B of exp * exp 
  | Not of exp                 (* !b *)
  | And of exp * exp          (* b0 /\ b1 *) (* b0 && b1 *) 
  | Or of exp * exp           (* b0 \/ b1 *) (* b0 || b1 *) 

  | HasProto1 of loc * exp_inside * loc
  | HasProto2 of loc * loc

(*string exp*)
  | Str of string
  | PPrint of loc 

type com =
  | Set of loc * exp           (* x := a *) 
  | Seq of com * com            (* c0 ; c1 *)
  | If of exp * com		(* if b then c end *)
  | Ifelse of exp * com * com  (* if b then c0 end else c1 end *)
  | While of exp * com         (* while b do c *)
  | Print of loc
  
  | Declareint of loc		(* Integer i *)
  | Declarebool of loc
  | Declarestr of loc
  | Declareproto of loc * loc * string * loc
  | Readfrom of loc * loc
  | Writeto of loc * loc
 
  | SetProto1 of loc * exp_inside * loc * exp
  | SetProto2 of loc * exp_inside * loc * index * exp
  | SetProto3 of loc * loc * exp
  | SetProto4 of loc * loc * index * exp

  | AddEle1 of loc * loc * exp  
  | AddEle2 of loc * exp_inside * loc * exp
  | AddEle3 of loc * loc
  | AddEle4 of loc * exp_inside * loc

  | Copyfrom of pro_ele * pro_ele
 

module P = Printf 

let rec index_to_str i = match i with
  | IndexConst l -> l
  | IndexIden l -> l

and exp_to_str a = match a with
(*arithm exp*)
  | Var l -> l
  | Const n -> n
  | Add (a, b) -> P.sprintf "(%s + %s)" (exp_to_str a) (exp_to_str b)
  | Sub (a, b) -> P.sprintf "(%s - %s)" (exp_to_str a) (exp_to_str b)
  | Mul (a, b) -> P.sprintf "(%s * %s)" (exp_to_str a) (exp_to_str b)
  | Div (a, b) -> P.sprintf "(%s / %s)" (exp_to_str a) (exp_to_str b)
  | Mod (a, b) -> P.sprintf "(%s %% %s)" (exp_to_str a) (exp_to_str b)
  
  | Protoele p -> P.sprintf "%s" (pro_ele_to_str p)
  | Sizeof1 (l1, e, l2) -> 
      P.sprintf "%s.%s->%s_size()" l1 (exp_inside_to_str e) l2
  | Sizeof2 (l1, l2) ->
      P.sprintf "%s.%s_size()" l1 l2
(*bool exp*)
  | True -> "true"
  | False -> "false"

  | EQ (a, b) -> P.sprintf "(%s == %s)" (exp_to_str a) (exp_to_str b)
  | LE (a, b) -> P.sprintf "(%s <= %s)" (exp_to_str a) (exp_to_str b)
  | BE (a, b) -> P.sprintf "(%s >= %s)" (exp_to_str a) (exp_to_str b)
  | L (a, b) -> P.sprintf "(%s < %s)" (exp_to_str a) (exp_to_str b)
  | B (a, b) -> P.sprintf "(%s > %s)" (exp_to_str a) (exp_to_str b)
  | Not b -> P.sprintf "!%s" (exp_to_str b)
  | And (a, b) -> P.sprintf "(%s && %s)" (exp_to_str a) (exp_to_str b)
  | Or (a, b) -> P.sprintf "(%s || %s)" (exp_to_str a) (exp_to_str b)
(*string exp*)
  | Str (s) -> P.sprintf "\"%s\"" s
  | PPrint (l) -> P.sprintf "%s.DebugString()" l
 
  | HasProto1 (l1, e, l2) ->
      P.sprintf "%s.%s->has_%s()" l1 (exp_inside_to_str e) l2
  | HasProto2 (l1, l2) ->
      P.sprintf "%s.has_%s()" l1 l2 


and pro_ele_to_str p = match p with
  | AccessProto1 (l1, e, l2) ->
      P.sprintf "%s.%s->%s()" l1 (exp_inside_to_str e) l2
  | AccessProto2 (l1, e, l2, a) ->
      P.sprintf "%s.%s->%s(%s)" l1 (exp_inside_to_str e) l2 (index_to_str a)
  | AccessProto3 (l1, l2) ->
      P.sprintf "%s.%s()" l1 l2
  | AccessProto4 (l1, l2, a) ->
      P.sprintf "%s.%s(%s)" l1 l2 (index_to_str a)

and com_to_str c = match c with
  | Set (l, a) -> P.sprintf "%s = %s" l (exp_to_str a)
  | Seq (a, b) -> P.sprintf "%s ;\n%s" (com_to_str a) (com_to_str b)
  | If (b, c) -> P.sprintf "if (%s) { %s ; }" (exp_to_str b) (com_to_str c)
  | Ifelse (b, c0, c1) ->
      P.sprintf "if (%s) { %s ; } \nelse { %s ; }"
	(exp_to_str b) (com_to_str c0) (com_to_str c1) 
  | While (b, c) ->
      P.sprintf "while %s {\n%s ;\n}" (exp_to_str b) (com_to_str c) 
  | Print l -> P.sprintf "cout << %s << endl" l
  | Declareint l -> P.sprintf "int %s" l
  | Declarebool l -> P.sprintf "bool %s" l
  | Declarestr l -> P.sprintf "string %s" l
  | Declareproto (l1, l2, s, l3) -> P.sprintf "%s %s" l3 l1
  | Readfrom (l1, l2) -> 
      P.sprintf "fstream input(\"%s\",ios::in | ios::binary);\n%s.ParseFromIstream(&input)" l2 l1
  | Writeto (l1, l2) -> 
      P.sprintf "fstream output(\"%s\",ios::out | ios::binary);\n%s.SerializeToOstream(&output)" l2 l1
 

  | SetProto1 (l1, e, l2, a) ->
      P.sprintf "%s.%s->set_%s(%s)" l1 (exp_inside_to_str e) l2 (exp_to_str a)
  | SetProto2 (l1, e, l2, a, b) ->
      P.sprintf "%s.%s->set_%s(%s,%s)" l1 (exp_inside_to_str e) l2 (index_to_str a) (exp_to_str b)
  | SetProto3 (l1, l2, a) ->
      P.sprintf "%s.set_%s(%s)" l1 l2 (exp_to_str a)
  | SetProto4 (l1, l2, a, b) ->
      P.sprintf "%s.set_%s(%s, %s)" l1 l2 (index_to_str a) (exp_to_str b)

  | AddEle1 (l1, l2, a) ->
      P.sprintf "%s.add_%s(%s)" l1 l2 (exp_to_str a)
  | AddEle2 (l1, e, l2, a) ->
      P.sprintf "%s.%s->add_%s(%s)" l1 (exp_inside_to_str e) l2 (exp_to_str a)
  | AddEle3 (l1, l2) ->
      P.sprintf "%s.add_%s()" l1 l2
  | AddEle4 (l1, e, l2) ->
      P.sprintf "%s.%s->add_%s()" l1 (exp_inside_to_str e) l2
  | Copyfrom (p1, p2) ->
      P.sprintf "%s = %s" (pro_ele_to_str p1) (pro_ele_to_str p2)


and exp_inside_to_str e = match e with
  | ExpEle1 (l) ->
      P.sprintf "mutable_%s()" l
  | ExpEle2 (l, a) ->
      P.sprintf "mutable_%s(%s)" l (index_to_str a)
  | ExpEle3 (e1, e2) ->
      P.sprintf "%s->%s" (exp_inside_to_str e1) (exp_inside_to_str e2)

let rec find_include c = match c with
  | Set (a, b) -> "" 
  | Seq (a, b) -> P.sprintf "%s%s" (find_include a) (find_include b)
  | If (b, c) -> P.sprintf "%s" (find_include c)
  | Ifelse (b, c0, c1) -> P.sprintf "%s\n%s" (find_include c0) (find_include c1)
  | While (b, c) -> P.sprintf "%s" (find_include c)
  | Print a -> ""
  | Declareint l -> ""
  | Declarebool l -> ""
  | Declarestr l -> ""
  | Declareproto (l1, l2, s, l3) -> P.sprintf "#include \"%s.pb.h\"\n" l2
  | Readfrom (l1, l2) -> ""
  | Writeto (l1, l2) -> ""
  | SetProto1 (l1, e,l2, a) -> ""
  | SetProto2 (l1, e, l2, a ,b) -> ""
  | SetProto3 (l1, l2, a) -> ""
  | SetProto4 (l1, l2, a, b) -> ""
  | AddEle1 (l1, l2, a) -> ""
  | AddEle2 (l1, e, l2, a) -> ""
  | AddEle3 (l1, l2) -> ""
  | AddEle4 (l1, e,l2) -> ""
  | Copyfrom (p1, p2) -> ""
