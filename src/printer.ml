(** Projet Polish -- Analyse statique d'un mini-langage impératif *)
(* Module permettant le réaffichage d'un programme Polish à partir de sa
syntaxe abstraite. *)

open AbstractSyntax;;

(* Prend une expression en forme abstraite et l'affiche en forme de string sur
stdout *)
let rec print_expr (e:expr) : unit =
  match e with
  | Num(x) -> Printf.printf "%d " x
  | Var(x) -> Printf.printf "%s " x
  | Op(op, x, y) ->
      let operator = match op with
        | Add -> "+ "
        | Sub -> "- "
        | Mul -> "* "
        | Div -> "/ "
        | Mod -> "% "
      in Printf.printf "%s" operator; print_expr x; print_expr y
;;

(* Prend une condition en forme abstraite et l'affiche en forme de string sur
stdout *)
let print_condition (c:cond) : unit =
  let (exp1, op, exp2) = c in
  print_expr exp1;
  let comparator = match op with
    | Eq -> "= "
    | Ne -> "<> "
    | Lt -> "< "
    | Le -> "<= "
    | Gt -> "> "
    | Ge -> ">= "
  in Printf.printf "%s" comparator;
  print_expr exp2;
;;


(* Prend une instruction en forme abstraite et l'affiche en forme de string sur
stdout *)
let rec print_instruction (instruction:instr) (level:int) : bool =
  Printf.printf "%s" (String.make (level*2) ' ');
  match instruction with
  | Set (name, exp) -> Printf.printf "%s := " name; print_expr exp; true
  | Read (name) -> Printf.printf "READ %s" name; true
  | Print (exp) -> Printf.printf "PRINT "; print_expr exp; true
  | If (condition, yes, no) ->
      Printf.printf "IF ";
      print_condition condition;
      Printf.printf "\n";
      print_block yes (level+1);
      if no != []
      then
        (
          Printf.printf "%sELSE\n" (String.make (level*2) ' ');
          print_block no (level+1);
          false
        )
      else
        false;

  | While (condition, code) ->
      (
        Printf.printf "WHILE ";
        print_condition condition;
        Printf.printf "\n";
        print_block code (level+1);
        false
      )

(* Prend un programme en entrée de forme d'une liste de tuples
(pos, liste de strings) et l'afficheen forme de string sur stdout. *)
and print_block (p:program) (level:int) : unit =
  match p with
  | [] -> ();
  | (pos, instruction)::rest ->
      let newline = print_instruction instruction level in
      if newline
      then Printf.printf "\n"
      else ();
      print_block rest level;
;;


let print_polish (p:program) : unit =
  print_block p 0
;;
