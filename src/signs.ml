open AbstractSyntax;;
open Set;;
open Printf;;
open String;;

type sign = Neg | Zero | Pos | Error

(* Analogue à Set.OrderedType, l'ordre est en réalité une relation de différence *)
module Signs =
  struct
    type t = sign
    let compare x y =
      if (x = y) then 0
      else -1
  end

module SignSet = Set.Make(Signs)
module SignMap = Map.Make(String)

let sign_of x =
  if x > 0 then
    Pos
  else if x < 0 then
    Neg
  else
    Zero
let zeroSet = SignSet.add Zero SignSet.empty
let posSet = SignSet.add Pos SignSet.empty
let negSet = SignSet.add Neg SignSet.empty
let errorSet = SignSet.add Error SignSet.empty
let allSet = SignSet.of_list [Zero; Neg; Pos]
let parse_add sign1 sign2 =
  if (SignSet.equal sign1 zeroSet) then (*Un des deux sets egal a zero*)
    sign2
  else if (SignSet.equal sign2 zeroSet) then
    sign1
  else if (SignSet.equal sign1 sign2) then (*Les sets sont egaux*)
    sign1
  else if (SignSet.cardinal sign2 == 2)  (*Si un des sets ne contient que 0*)
          && SignSet.equal (SignSet.remove Zero sign2) sign1 then  (*et un autre opérateur,*)
    sign1                             (* et que l'autre ne contient que cette opérateur*)
  else if (SignSet.cardinal sign1 == 2)
          && SignSet.equal (SignSet.remove Zero sign1) sign2 then
    sign2
  else SignSet.union zeroSet (SignSet.union sign1 sign2) (* Sinon l'union *)

let parse_op op sign1 sign2 =
  match op with
  | Add -> parse_add sign1 sign2
  | Sub -> SignSet.empty
  | Mul -> SignSet.empty
  | Div -> SignSet.empty
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
  | Read(name) -> sign_polish rest (SignMap.add name allSet env)
  | Set(name, e) -> sign_polish rest (SignMap.add name (parse_expr e env) env)
  | Print(_) -> sign_polish rest env