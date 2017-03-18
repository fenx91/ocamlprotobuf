open Imp

module P = Printf 

module StringMap = Map.Make(String)

type env = string StringMap.t 
let empty_env: env = StringMap.empty

let lookup (en: env) (l :loc) : string = 
  try
    StringMap.find l en
  with Not_found -> ""	


let rec check_exp (e: exp) (en: env): string = match e with
  | Const n -> "Integer"
  | Var(loc) -> let t = lookup en loc in
    if (t = "") then failwith ("Undefined Protobuf variable: " ^ loc) else t
 
  | Add(a1, a2) -> let t1 = (check_exp a1 en) in
                   let t2 = (check_exp a2 en) in 
    if (t1 = "Integer" || t1 = "Proto_ele") && (t2 = "Integer" || t2 = "Proto_ele")
    then "Integer" else failwith ("Add operator works on Number types only!")
  | Sub(a1, a2) -> let t1 = (check_exp a1 en) in
                   let t2 = (check_exp a2 en) in
    if (t1 = "Integer" || t1 = "Proto_ele") && (t2 = "Integer" || t2 = "Proto_ele")
    then "Integer" else failwith ("Subtract operator works on Number types only!")
  | Mul(a1, a2) -> let t1 = (check_exp a1 en) in
                   let t2 = (check_exp a2 en) in
    if (t1 = "Integer" || t1 = "Proto_ele") && (t2 = "Integer" || t2 = "Proto_ele")
    then "Integer" else failwith ("Multiply operator works on Number types only!")
  | Div(a1, a2) -> let t1 = (check_exp a1 en) in
                   let t2 = (check_exp a2 en) in
    if (t1 = "Integer" || t1 = "Proto_ele") && (t2 = "Integer" || t2 = "Proto_ele")
    then "Integer" else failwith ("Divide operator works on Number types only!")
  | Mod(a1, a2) -> let t1 = (check_exp a1 en) in
                   let t2 = (check_exp a2 en) in
    if (t1 = "Integer" || t1 = "Proto_ele") && (t2 = "Integer" || t2 = "Proto_ele")
    then "Integer" else failwith ("Mod operator works on Number types only!")

  | Protoele(p) ->  "Proto_ele"
  | Sizeof1 (l1, e, l2) ->  let t1 = (lookup en l1) in 
        if not(t1 = "Proto") then failwith ("sizeof works on Protobuf variable only!") else "Integer"
  | Sizeof2 (l1, l2) ->  let t1 = (lookup en l1) in
        if not(t1 = "Proto") then failwith ("sizeof works on Protobuf variable only!") else "Integer"

  | False -> "Bool"
  | True -> "Bool"
  | EQ (a, b) -> let t1 = check_exp a en in
                 let t2 = check_exp b en in
        if (t1 = "Integer" || t1 = "Proto_ele") && (t2 = "Integer" || t2 = "Prot_ele")
        then "Bool" else failwith ("Comparison operators can work on Number types only!")
  | LE (a, b) -> let t1 = check_exp a en in
                 let t2 = check_exp b en in
        if (t1 = "Integer" || t1 = "Proto_ele") && (t2 = "Integer" || t2 = "Prot_ele")
        then "Bool" else failwith ("Comparison operators can work on Number types only!")
  | BE (a, b) -> let t1 = check_exp a en in
                 let t2 = check_exp b en in
        if (t1 = "Integer" || t1 = "Proto_ele") && (t2 = "Integer" || t2 = "Prot_ele")
        then "Bool" else failwith ("Comparison operators can work on Number types only!")
  | L (a, b) -> let t1 = check_exp a en in
                 let t2 = check_exp b en in
        if (t1 = "Integer" || t1 = "Proto_ele") && (t2 = "Integer" || t2 = "Prot_ele")
        then "Bool" else failwith ("Comparison operators can work on Number types only!")
  | B (a, b) -> let t1 = check_exp a en in
                 let t2 = check_exp b en in
        if (t1 = "Integer" || t1 = "Proto_ele") && (t2 = "Integer" || t2 = "Prot_ele")
        then "Bool" else failwith ("Comparison operators can work on Number types only!")
  | Not b -> let t = check_exp b en in 
        if (t = "Bool" || t = "Proto_ele") then "Bool" else
        failwith ("Logical operators can work on boolean types only!") 
  | And (b1, b2) -> let t2 = check_exp b1 en in
                    let t1 = check_exp b2 en in
        if (t1 = "Bool" || t1 = "Proto_ele") && (t2 = "Bool" || t2 = "Proto_ele")  then "Bool" else
        failwith ("Logical operators can work on boolean types only!") 
  | Or (b1, b2) -> let t2 = check_exp b1 en in
                    let t1 = check_exp b2 en in
        if (t1 = "Bool" || t1 = "Proto_ele") && (t2 = "Bool" || t2 = "Proto_ele")  then "Bool" else
        failwith ("Logical operators can work on boolean types only!") 
 
  | Str (s) -> "String"
  | PPrint (l) -> let t = lookup en l in 
        if (t = "Proto") then "String" else
        failwith ("PrettyPrint works on Protobuf variables only!")

let rec check_com (c: com) (en: env): env = match c with
  | Set (l, a) -> let t = lookup en l in
     if t = "" then failwith ("Undefined variable: " ^ l) else 
     if not (t = (check_exp a en)) then failwith ("Cannot assign to a different type!") else en
  | Seq (a, b) -> check_com b (check_com a en)
  | If (b, c) -> en
  | Ifelse (b, c0, c1) -> en
  | While (b, c) -> en
  | Print l -> en
  | Declareint l -> StringMap.add l "Integer" en
  | Declarebool l -> StringMap.add l "Bool" en
  | Declarestr l -> StringMap.add l "String" en
  | Declareproto (l1, l2, s, l3) -> if not (s = "proto") then failwith ("Please provide the correct .proto file!") else
     StringMap.add l1 "Proto" en

  | SetProto1 (l1, e, l2, a) -> let t = lookup en l1 in
     if (t = "") then failwith ("Undefined Protobuf variable: " ^ l1) else
     if not (t = "Proto") then failwith (l1 ^ " is not a Protobuf variable!") else en

  | SetProto2 (l1, e, l2, a, b) -> let t = lookup en l1 in
     if (t = "") then failwith ("Undefined Protobuf variable: " ^ l1) else
     if not (t = "Proto") then failwith (l1 ^ " is not a Protobuf variable!") else en
 
  | SetProto3 (l1, l2, a) -> let t = lookup en l1 in
     if (t = "") then failwith ("Undefined Protobuf variable: " ^ l1) else
     if not (t = "Proto") then failwith (l1 ^ " is not a Protobuf variable!") else en

  | SetProto4 (l1, l2, a, b) -> let t = lookup en l1 in
     if (t = "") then failwith ("Undefined Protobuf variable: " ^ l1) else
     if not (t = "Proto") then failwith (l1 ^ " is not a Protobuf variable!") else en


 



