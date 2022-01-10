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

let merge_environments key ss1 ss2 =
  Some (SignSet.union ss1 ss2)
;;

let rec fixed_point_while_analysis c env bl =
  let cSat, _, tEnv, _ = signs_propagate_condition c env in
  let toJoin =
    if cSat
    then let tJEnv, _ = signs_bloc bl Int.max_int Int.max_int tEnv in tJEnv
    else SignMap.empty
  in
  let nextEnv = SignMap.union merge_environments env toJoin in
  if SignMap.equal (fun s1 s2 -> SignSet.equal s1 s2) env nextEnv
  then nextEnv
  else fixed_point_while_analysis c nextEnv bl
and signs_bloc (p : program) (divZeroLine : int) (prevpos : int) (env : SignSet.t SignMap.t) =
  let divZeroLine =
    if SignMap.exists (fun key el -> SignSet.mem Sign.Err el) env
    then min divZeroLine prevpos
    else divZeroLine
  in
  if p = [] then
		env, divZeroLine
	else
		let pos, instr = List.hd p in
		let rest = List.tl p in
    match instr with
    | If(cond, tb, fb) ->
      let tbSat, fbSat, tEnv, fEnv = signs_propagate_condition cond env in
      let newEnv1, newDivZ =
        if tbSat
        then signs_bloc tb Int.max_int (pos+1) tEnv
        else SignMap.empty, divZeroLine
      in
      let divZeroLine = min divZeroLine newDivZ in
      let newEnv2, newDivZ =
        if fbSat
        then signs_bloc fb Int.max_int (pos+1) fEnv
        else SignMap.empty, divZeroLine
      in
      let divZeroLine = min divZeroLine newDivZ in
      (SignMap.union merge_environments newEnv1 newEnv2) |> signs_bloc rest divZeroLine pos
    | While(cond, b) ->
      let newEnv = fixed_point_while_analysis cond env b in
      signs_bloc rest divZeroLine pos newEnv
    | Read(name) -> signs_bloc rest divZeroLine pos (SignMap.add name all env) 
    | Set(name, e) -> signs_bloc rest divZeroLine pos (SignMap.add name (signs_expr e env) env)
    | Print(e) -> signs_bloc rest divZeroLine pos env
;;

let signset_to_string s =
  let signToString si =
    match si with
    | Sign.Pos -> "+"
    | Sign.Neg -> "-"
    | Sign.Zero -> "0"
    | Sign.Err -> "!"
  in
  SignSet.fold (fun e acc -> acc ^ (signToString e)) s ""
;;

let print_env e =
  SignMap.iter (fun k v -> printf "%s %s\n" k (signset_to_string v)) e
;;

let signs_polish (p : program) =
  let env, divZero = signs_bloc p (Int.max_int) 0 SignMap.empty in
  print_env env;
  print_string "\n";
  if divZero == Int.max_int
  then print_string "safe\n"
  else printf "possible division by zero\n" (*(divZero+1)*);
;;

(* let test =
  let c = Var("a"), Ne, Var("b") in
  let e = SignMap.empty |> SignMap.add "a" pos in
  let e = e |> SignMap.add "b" neg in
  let cSat, necSat, tenv, fenv = signs_propagate_condition c e in
  printf "%B\n%B\n" cSat necSat;
  print_env tenv;
  print_env fenv;
;; *)

