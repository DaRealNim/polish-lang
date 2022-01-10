open AbstractSyntax;;
open SignsOp;;
open Set;;
open Printf;;
open String;;

module SignSet = Set.Make(Sign);;
module SignMap = Map.Make(String);;

(** Environement de signes : une map de sets de signes*)
type signenvironment = SignSet.t SignMap.t;;

(** Retourne le signe du type Sign.t de l'entiers x*)
let sign_of x =
  if x > 0 then
    Sign.Pos
  else if x < 0 then
    Sign.Neg
  else
    Sign.Zero
;;

(** Récupère l'ensemble de signes possibles resultant d'une opération op
sur deux expressions ayant pour signes possibles les ensembles de signes sign1 et sign2*)
let signs_op op sign1 sign2 =
  match op with
  | Add -> get_add_sign sign1 sign2
  | Sub -> get_sub_sign sign1 sign2
  | Mul -> get_mul_sign sign1 sign2
  | Div -> get_div_sign sign1 sign2
  | Mod -> get_mod_sign sign1 sign2
;;
  
(** Retourne un ensemble de signes possibles d'une expression selon un environnement*)
let rec signs_expr e env =
  match e with
		| Var(name) -> SignMap.find name env
    | Num(x) -> SignSet.add (sign_of x) SignSet.empty 
		| Op(op,expr1,expr2) -> signs_op op (signs_expr expr1 env) (signs_expr expr2 env)
;;

(** Une liste qui associe un couple de signes (a,b) a deux ensembles de signes (s1,s2) 
pour l'opération a < b *)
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


(** Fais l'union des résultats d'une opération sur les signes a et b,
ou a et b sont toutes les combinaisons des ensembles de signes s1 et s2.
Le résultat est donné par la liste d'association li *)
let get_assoc li s1 s2 =
  let s1 = SignSet.remove Err s1 in
  let s2 = SignSet.remove Err s2 in
  let s1', s2' = SignSet.fold
    (fun sign1 acc ->
      SignSet.fold
      (fun sign2 acc2 ->
        let accSet1, accSet2 = acc2 in
        let toUnion1, toUnion2 = List.assoc (sign1, sign2) li in
        (SignSet.union accSet1 toUnion1, SignSet.union accSet2 toUnion2)
      ) s2 acc
    )
    s1 (SignSet.empty, SignSet.empty) 
  in (s1', s2')
;;

(** Retourne deux ensembles de signes correspondant aux signes des expressions e1 et e2 dont
les signes possibles sont les ensembles s1 et s2, après application de l'opérateur compa*)
let rec spc_aux_signsets s1 compa s2 =
  let revpair = fun (a, b) -> (b, a) in
  match compa with
  | Eq -> let a = (SignSet.inter s1 s2) in a, a
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

(** Propage une condition sur un environnement de signes.
Retourne bool * bool * env * env, le premier booleen = true si la condition est
satisfiable dans l'environnement donné, le deuxième booleen = true si la négation
de la condition est satisfiable, le premier environnement est l'environnement correspondant à
la réussite de la condition, a appliquer dans le bloc "vrai" du if (ou du while),
et le deuxième correspondant à l'échec de la condition, a appliquer dans le bloc "faux"
du if.*)
let signs_propagate_condition c (env : signenvironment) : bool * bool * signenvironment * signenvironment  =
  let e1, compa, e2 = c in
  let s1 = signs_expr e1 env in
  let s2 = signs_expr e2 env in
  let invCompa = compa |> invert_comparator in
  let trueSet1, trueSet2 = spc_aux_signsets s1 compa s2 in
  let falseSet1, falseSet2 = spc_aux_signsets s1 invCompa s2 in
  let condSat = trueSet1 <> SignSet.empty && trueSet2 <> SignSet.empty in
  let invCondSat = falseSet1 <> SignSet.empty && falseSet2 <> SignSet.empty in
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

(** Fonction utilitaire a passer a un SignMap.union pour la fusion de deux
environnements*)
let merge_environments key ss1 ss2 =
  Some (SignSet.union ss1 ss2)
;;

(** Execute une analyse en point fixe d'un bloc while, en prenant sa condition, son bloc
de code, et l'environnement de départ. Propage la condition sur le bloc de code jusqu'a
ce que l'environnement ne change plus *)
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

(** Détermine un environnement de signes en fonction d'un bloc de code polish.
Retourne un environnement, ainsi que la position de la première possible division par 0
(non stable)*)
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
      (* On propage la condition sur les blocs true et false.*)
      let tbSat, fbSat, tEnv, fEnv = signs_propagate_condition cond env in
      let newEnv1, newDivZ =
        (* Si le bloc true est satisfiable, on relance signs_bloc sur le bloc
        avec le nouvel environnement, sinon, on renvoie le set vide*)
        if tbSat
        then signs_bloc tb Int.max_int (pos+1) tEnv
        else SignMap.empty, divZeroLine
      in
      let divZeroLine = min divZeroLine newDivZ in
      let newEnv2, newDivZ =
        (* Idem *)
        if fbSat
        then signs_bloc fb Int.max_int (pos+1) fEnv
        else SignMap.empty, divZeroLine
      in
      let divZeroLine = min divZeroLine newDivZ in
      (* On fusionne les deux environnements générés par le if et on continue l'analyse*)
      (SignMap.union merge_environments newEnv1 newEnv2) |> signs_bloc rest divZeroLine pos
    | While(cond, b) ->
      let newEnv = fixed_point_while_analysis cond env b in
      signs_bloc rest divZeroLine pos newEnv
    | Read(name) -> signs_bloc rest divZeroLine pos (SignMap.add name all env) 
    | Set(name, e) -> signs_bloc rest divZeroLine pos (SignMap.add name (signs_expr e env) env)
    | Print(e) -> signs_bloc rest divZeroLine pos env
;;

(** Convertit un ensemble de signes en une chaine de caractères avec la représentation
de chaque signe (+, -, 0, !)*)
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

(** Affiche un environnement de signes, une variable par ligne*)
let print_env e =
  SignMap.iter (fun k v -> printf "%s %s\n" k (signset_to_string v)) e
;;

(** Lance l'analyse statique du programme passée et affiche l'environnement de signes
obtenu, ainsi que si une division par zéro est possible*)
let signs_polish (p : program) =
  let env, divZero = signs_bloc p (Int.max_int) 0 SignMap.empty in
  print_env env;
  print_string "\n";
  if divZero == Int.max_int
  then print_string "safe\n"
  else printf "possible division by zero\n" (*(divZero+1)*);
;;
