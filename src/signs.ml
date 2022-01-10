open AbstractSyntax;;
open SignsOp;;
open Set;;
open Printf;;
open String;;

module SignSet = Set.Make(Sign);;
module SignMap = Map.Make(String);;

type signenvironment = SignSet.t SignMap.t;;

let sign_of x =
  if x > 0 then
    Sign.Pos
  else if x < 0 then
    Sign.Neg
  else
    Sign.Zero
;;

let signs_op op sign1 sign2 =
  match op with
  | Add -> get_add_sign sign1 sign2
  | Sub -> get_sub_sign sign1 sign2
  | Mul -> get_mul_sign sign1 sign2
  | Div -> get_div_mod_sign sign1 sign2 false
  | Mod -> get_div_mod_sign sign1 sign2 true
;;
  
let rec signs_expr e env =
  match e with
		| Var(name) -> SignMap.find name env
    | Num(x) -> SignSet.add (sign_of x) SignSet.empty 
		| Op(op,expr1,expr2) -> signs_op op (signs_expr expr1 env) (signs_expr expr2 env)
;;

let spc_lt_tab =
  [
    ((Sign.Pos, Sign.Neg), (SignSet.empty, SignSet.empty));
    ((Sign.Pos, Sign.Pos), (pos, pos));
    ((Sign.Neg, Sign.Pos), (neg, pos));
    ((Sign.Neg, Sign.Neg), (neg, neg));
    ((Sign.Zero, Sign.Pos), (zero, pos));
    ((Sign.Pos, Sign.Zero), (SignSet.empty, SignSet.empty));
    ((Sign.Zero, Sign.Neg), (SignSet.empty, SignSet.empty));
    ((Sign.Neg, Sign.Zero), (neg, zero));
    ((Sign.Zero, Sign.Zero), (SignSet.empty, SignSet.empty));
  ];;


  (* +-0 < -0*)
let get_assoc li s1 s2 =
  let s1 = SignSet.remove Err s1 in
  let s2 = SignSet.remove Err s2 in
  let s1', s2' = SignSet.fold
    (fun sign1 acc ->
      SignSet.fold
      (fun sign2 acc2 ->
        let s1', s2' = acc2 in
        let toUnion1, toUnion2 = List.assoc (sign1, sign2) li in
        (SignSet.union s1' toUnion1, SignSet.union s2' toUnion2)
      ) s2 acc
    )
    s1 (SignSet.empty, SignSet.empty)
  in (s1', s2')
;;

(* +-0  <  -0*)
let rec spc_aux_signsets s1 compa s2 =
  let revpair = fun (a, b) -> (b, a) in
  match compa with
  | Eq -> let a = (SignSet.inter s2 s1) in a, a
  | Ne -> let a = SignSet.union (SignSet.diff s1 s2) (SignSet.diff s2 s1) in a,a
  | Lt -> get_assoc spc_lt_tab s1 s2
  | Gt -> revpair (get_assoc spc_lt_tab s2 s1)
  | Le ->
    let s1', s2' = spc_aux_signsets s1 Eq s2 in
    let s1'', s2'' = spc_aux_signsets s1 Lt s2 in
    (SignSet.union s1' s1''), (SignSet.union s2' s2'')
  | Ge -> 
    let s1', s2' = spc_aux_signsets s1 Eq s2 in
    let s1'', s2'' = spc_aux_signsets s1 Gt s2 in
    (SignSet.union s1' s1''), (SignSet.union s2' s2'')
;;

(** Va renvoyer bool * bool * env * env, le premier booleen = true si la condition est
satisfiable dans l'environement donné, le deuxième booleen = true si la négation
de la condition est satisfiable, le premier environement est celui a
appliquer dans le bloc "vrai" du if (ou du while), et le deuxième dans le
bloc "faux" *)
let signs_propagate_condition c (env : signenvironment) : bool * bool * signenvironment * signenvironment  =
  let e1, compa, e2 = c in
  let s1 = signs_expr e1 env in
  let s2 = signs_expr e2 env in
  let invCompa = compa |> invert_comparator in
  let trueSet1, trueSet2 = spc_aux_signsets s1 compa s2 in
  let falseSet1, falseSet2 = spc_aux_signsets s1 invCompa s2 in
  let condSat = trueSet1 <> SignSet.empty || trueSet2 <> SignSet.empty in
  let invCondSat = falseSet1 <> SignSet.empty || falseSet2 <> SignSet.empty in
  let trueEnv, falseEnv =
    match e1, e2 with
    | Var(v1), Var(v2) ->
      (SignMap.add v1 trueSet1 env) |> SignMap.add v2 trueSet2,
      (SignMap.add v1 falseSet1 env) |> SignMap.add v2 falseSet2
    | Var(v1), _ ->
      (SignMap.add v1 trueSet1 env),
      (SignMap.add v1 falseSet1 env)
    | _, Var(v2) ->
      (SignMap.add v2 trueSet1 env),
      (SignMap.add v2 falseSet1 env)
    | _, _ -> env, env
  in
  condSat, invCondSat, trueEnv, falseEnv
;;
  
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
  | Set(name, e) -> sign_polish rest (SignMap.add name (signs_expr e env) env)
  | Print(_) -> sign_polish rest env
;;

let test =
  let c = Var("a"), Ne, Var("b") in
  let e = SignMap.empty |> SignMap.add "a" pos in
  let e = e |> SignMap.add "b" neg in
  let cSat, necSat, tenv, fenv = signs_propagate_condition c e in
  printf "%B\n%B\n" cSat necSat;
  print_string "a in true env: ";
  (SignMap.find "a" tenv) |> print_sign_set;
  print_string "b in true env: ";
  (SignMap.find "b" tenv) |> print_sign_set;
  print_string "a in false env: ";
  (SignMap.find "a" fenv) |> print_sign_set;
  print_string "b in false env: ";
  (SignMap.find "b" fenv) |> print_sign_set;
;;

