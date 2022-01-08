open AbstractSyntax;;
open SignsOp;;
open Set;;
open Printf;;
open String;;

module SignSet = Set.Make(Sign);;
module SignMap = Map.Make(String);;

let sign_of x =
  if x > 0 then
    Pos
  else if x < 0 then
    Neg
  else
    Zero

let parse_op op sign1 sign2 =
  match op with
  | Add -> get_add_sign sign1 sign2
  | Sub -> get_sub_sign sign1 sign2
  | Mul -> get_mul_sign sign1 sign2
  | Div -> get_div_sign sign1 sign2
  | Mod -> SignSet.empty
  
let rec parse_expr e env =
  match e with
		| Var(name) -> SignMap.find name env
    | Num(x) -> SignSet.add (sign_of x) SignSet.empty 
		| Op(op,expr1,expr2) -> parse_op op (parse_expr expr1 env) (parse_expr expr2 env)

let rec sign_polish (p : program) (env : SignSet.t SignMap.t) =
  if p = [] then
		env
	else
		let _, instr = List.hd p in
		let rest = List.tl p in
  match instr with
  | If(cond, tb, fb) -> SignMap.empty
  | While(cond, b) -> SignMap.empty
  | Read(name) -> sign_polish rest (SignMap.add name all env)
  | Set(name, e) -> sign_polish rest (SignMap.add name (parse_expr e env) env)
  | Print(_) -> sign_polish rest env