(** Projet Polish -- Analyse statique d'un mini-langage impératif *)
(* Machine virtuelle permettant d'executer un programme polish en syntaxe
abstraite, générée par le Parser. *)

open AbstractSyntax;;

module Environment = Map.Make(String);;

(* Tente l'accès à la valeur de la variable var dans l'environment e. Affiche
une erreur si la variable n'a pas été ininialisée précédemment*)
let get_variable (var:name) (e:int Environment.t) =
  let v = Environment.find_opt var e in
  match v with
  | Some(value) -> value
  | None -> failwith (Printf.sprintf "Error: accessed variable %s before setting it" var)
;;

(* Prend une opération/expression en forme abstraite, l'évalue en fonction de
l'environement, et renvoie sa valeur *)
let rec eval_operation (operator:op) (ex1:expr) (ex2:expr) (env:int Environment.t) =
  match operator with
  | Add -> (eval_expression ex1 env) + (eval_expression ex2 env)
  | Sub -> (eval_expression ex1 env) - (eval_expression ex2 env)
  | Mul -> (eval_expression ex1 env) * (eval_expression ex2 env)
  | Div ->
      let valex2 = (eval_expression ex2 env) in
      if (valex2 = 0)
      then failwith "Error: tried dividing by 0"
      else (eval_expression ex1 env) / valex2
  | Mod ->
      let valex2 = (eval_expression ex2 env) in
      if (valex2 = 0)
      then failwith "Error: tried dividing by 0"
      else (eval_expression ex1 env) mod valex2

and eval_expression (ex:expr) (env:int Environment.t) =
  match ex with
  | Num(v) -> v
  | Var(variable) -> get_variable variable env
  | Op(operator, ex1, ex2) -> eval_operation operator ex1 ex2 env
;;

(* Prend une condition en forme abstraite, l'évalue en fonction de
l'environement et renvoie sa valeur booléenne *)
let eval_condition (c:cond) (env:int Environment.t) =
  match c with
  | (exp1, comparator, exp2) ->
      let valexp1 = eval_expression exp1 env in
      let valexp2 = eval_expression exp2 env in
      match comparator with
      | Eq -> valexp1 = valexp2
      | Ne -> valexp1 <> valexp2
      | Lt -> valexp1 < valexp2
      | Le -> valexp1 <= valexp2
      | Gt -> valexp1 > valexp2
      | Ge -> valexp1 >= valexp2
;;

(* Exécute une insruction en forme abstraite*)
let rec eval_instruction (i:instr) (e:int Environment.t) =
  match i with
  | Set (n, ex) -> Environment.add n (eval_expression ex e) e
  | Read (n) ->
      (
        Printf.printf "%s?" n;
        try
          let v = read_int() in
          Environment.add n v e
        with Failure s -> failwith (Printf.sprintf "Error: failed reading value for %s: %s" n s)
      )
  | Print (exp) -> Printf.printf "%d\n" (eval_expression exp e); e
  | If (cond, blockTrue, blockFalse) ->
      if (eval_condition cond e)
      then eval_block blockTrue e
      else eval_block blockFalse e
  | While (cond, blockRepeat) ->
      if (eval_condition cond e)
      then (eval_block blockRepeat e) |> eval_instruction i
      else e
(* Exécute les insructions d'un bloc *)
and eval_block (b:block) (e:int Environment.t) =
  match b with
  | [] -> e
  | (pos, inst)::remaining -> (eval_instruction inst e) |> eval_block remaining

;;

let eval_polish (p:program) : unit =
  let env = Environment.empty in
  let _ = eval_block p env in
  ()
;;
