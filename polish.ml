
(** Projet Polish -- Analyse statique d'un mini-langage impératif *)

(** Note : cet embryon de projet est pour l'instant en un seul fichier
    polish.ml. Il est recommandé d'architecturer ultérieurement votre
    projet en plusieurs fichiers source de tailles raisonnables *)

(*****************************************************************************)
(** Syntaxe abstraite Polish (types imposés, ne pas changer sauf extensions) *)

(** Position : numéro de ligne dans le fichier, débutant à 1 *)
type position = int

(** Nom de variable *)
type name = string

(** Opérateurs arithmétiques : + - * / % *)
type op = Add | Sub | Mul | Div | Mod

(** Expressions arithmétiques *)
type expr =
  | Num of int
  | Var of name
  | Op of op * expr * expr

(** Opérateurs de comparaisons *)
type comp =
| Eq (* = *)
| Ne (* Not equal, <> *)
| Lt (* Less than, < *)
| Le (* Less or equal, <= *)
| Gt (* Greater than, > *)
| Ge (* Greater or equal, >= *)

(** Condition : comparaison entre deux expressions *)
type cond = expr * comp * expr

(** Instructions *)
type instr =
  | Set of name * expr
  | Read of name
  | Print of expr
  | If of cond * block * block
  | While of cond * block
and block = (position * instr) list

(** Un programme Polish est un bloc d'instructions *)
type program = block


(***********************************************************************)

let read_polish (filename:string) : program = failwith "TODO"

let print_polish (p:program) : unit =
    let rec print_expr (e:expr) : unit =
        match e with
        | Num(x) -> Printf.printf " %d " x
        | Var(x) -> Printf.printf " %s " x
        | Op(op, x, y) ->
            let operator = match op with
            | Add -> " + "
            | Sub -> " - "
            | Mul -> " * "
            | Mod -> " % "
            in Printf.printf "%s" operator; print_expr x; print_expr y

    in
    let rec print_condition (c:cond) : unit =
        (* TODO *)
    in
    let rec print_block (p:program) (level:int) : unit =
        match p with
        | [] -> ();
        | (pos, instruction)::rest ->
            Printf.printf "%s" String.make (level*2) " ";
            match instruction with
            | Set (name, exp) -> Printf.printf "%s :=" name; print_expr exp;
            | Read (name) -> Printf.printf "READ %s" name;
            | Print (exp) -> Printf.printf "PRINT "; print_expr exp;
            | If (condition, yes, no) ->
                Printf.printf "IF ";
                print_condition condition;
                Printf.print "\n";
                print_block yes (level+1);
                Printf.print "ELSE";
                print_block no (level+1);
            | While (condition, code) ->
                Printf.printf "WHILE ";
                print_condition condition;
                Printf.print "\n";
                print_block code (level+1);


    in
    aux p 0
;;

let eval_polish (p:program) : unit = failwith "TODO"

let usage () =
  print_string "Polish : analyse statique d'un mini-langage\n";
  print_string "usage: à documenter (TODO)\n"

let main () =
  match Sys.argv with
  | [|_;"--reprint";file|] -> print_polish (read_polish file)
  | [|_;"--eval";file|] -> eval_polish (read_polish file)
  | _ -> usage ()

(* lancement de ce main *)
let () = main ()
