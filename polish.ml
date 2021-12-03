
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

exception WrongIndentation;;
exception ElseWithoutIf;;
exception WrongCondition;;
exception WrongExpression;;

(***********************************************************************)
let read_polish (filename:string) : program =
  let in_channel = open_in filename in

  let try_read () =
    try Some (input_line in_channel) with End_of_file -> None
  in

  let rec read_loop acc i =
    match try_read () with
    | Some s -> read_loop (i + 1, s)::acc
    | None -> close_in in_channel; List.rev acc
  in

  let main_line_list = loop [] 0
  in

  let count_spaces l count =
    match l with
    | " "::xs -> count_spaces xs (count + 1)
    | x::[] -> count
  in

  let rec cut_n_elements l n =
    if n > 0
    then cut_n_elements (List.tl l) (n-1)
    else l
  in
  (**
     type expr =
      | Num of int
      | Var of name
      | Op of op * expr * expr

     + 3 * 2 1
     + + 3 * 2 1

     Op(Add, Num(3), Op(Mul, Num(2), Num(1)))

     + 3 * 2 1

     match + - * / %
     sinon int_of_string
     sinon Var(nom)
     Add | Sub | Mul | Div | Mod
  *)
  let rec read_expr e =
      match e with
          | [] -> raise WrongExpression
          | "+"::xs ->
              let ex1, suite = read_expr xs in
              let ex2, fin = read_expr suite in
              Op(Add, ex1, ex2), fin
          | "-"::xs ->
              let ex1, suite = read_expr xs in
              let ex2, fin = read_expr suite in
              Op(Sub, ex1, ex2), fin
          | "*"::xs ->
              let ex1, suite = read_expr xs in
              let ex2, fin = read_expr suite in
              Op(Mul, ex1, ex2), fin
          | "/"::xs ->
              let ex1, suite = read_expr xs in
              let ex2, fin = read_expr suite in
              Op(Div, ex1, ex2), fin
          | "%"::xs ->
              let ex1, suite = read_expr xs in
              let ex2, fin = read_expr suite in
              Op(Mod, ex1, ex2), fin
          | x::xs -> (
              try
                Num(int_of_string x), xs
              with Failure "int_of_string" -> Var(x), xs
        )
  in
  let read_condition c =
      let ex1, reste = read_expr c in
      let comparator, reste = match reste with
        | "="::xs -> Eq, xs
        | "<>"::xs -> Ne, xs
        | "<"::xs -> Lt, xs
        | ">"::xs -> Gt, xs
        | "<="::xs -> Le, xs
        | ">="::xs -> Ge, xs
        | _ -> raise (WrongCondition "Invalid first expression")
      in
      let ex2, reste = read_expr reste in
      if reste = []
      then (ex1, comparator, ex2)
      else raise (WrongCondition "Invalid second expression")

  in
  let rec read_block lines depth =
    (* Si l'indentation est descendue, alors terminer*)

    let words = String.split_on_char ' ' line in

    (* TODO : compter espaces en début de line pour déterminer indentation
       ET supprimer les espaces de début de ligne*)

    let nbspaces = count_spaces words 0 in

    if (nbspaces / 2) > depth then
      failwith WrongIndentation
    else if (nbspaces / 2) < depth then
      (* Block fini *)
      []
    else
      let words_without_indentation = cut_n_elements words nbspaces
      in
      match lines with
      | [] -> []
      | (pos, line)::xs -> (pos, (read_instruction lines xs depth))::(read_block xs depth)
  and let rec read_instruction line next depth =
        match line with
        | [] -> []
        | "IF"::c -> If(cond, )
        | "READ"::name -> Read(name)
        | var::":="::expr -> Set(var, read_expr expr)
        | "WHILE"::c -> While(read_condition c, read_block next)
        | "ELSE"::reste -> failwith ElseWithoutIf
    in
    read_block main_line_list [] 0
;;

let print_polish (p:program) : unit =
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

  in
  let rec print_condition (c:cond) : unit =
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
  in
  let rec print_block (p:program) (level:int) : unit =
    match p with
    | [] -> ();
    | (pos, instruction)::rest ->
        Printf.printf "%s" (String.make (level*2) ' ');
        (match instruction with
         | Set (name, exp) -> Printf.printf "%s := " name; print_expr exp;
         | Read (name) -> Printf.printf "READ %s" name;
         | Print (exp) -> Printf.printf "PRINT "; print_expr exp;
         | If (condition, yes, no) ->
             Printf.printf "IF ";
             print_condition condition;
             Printf.printf "\n";
             print_block yes (level+1);
             if no != []
             then
               (
                 Printf.printf "ELSE\n";
                 print_block no (level+1);
               )

             else
               ();

         | While (condition, code) ->
             Printf.printf "WHILE ";
             print_condition condition;
             Printf.printf "\n";
             print_block code (level+1);
             Printf.printf "\n";
        );
        Printf.printf "\n";
        print_block rest level;
  in
  print_block p 0
;;



module Environment = Map.Make(String);;
let eval_polish (p:program) : unit =
    let rec eval_expression (e:expr) =
        (*TODO*)
    in
    let rec eval_condition (c:cond) =
        (*TODO*)
    in
    let rec eval_instruction (i:instr) (e:int Environment.t) =
        match i with
        | Set (n, val) -> Environment.add n val e
        | Read (n) -> (*TODO*)
        | Print (exp) -> Printf.printf "%d" eval_expression(exp); e
        | If (cond, blockTrue, blockFalse) ->
            (*ERROR : these must be mutually recursive (TODO)*)
            if (eval_condition cond)
            then eval_block blockTrue
            else eval_block blockFalse
        | While (cond, blockRepeat) ->
            (*ERROR: same (TODO)*)
            if (eval_condition cond)
            then eval_block blockRepeat |> eval_instruction i
            else e

    in
    let rec eval_block (b:block) (e:int Environment.t) =
        match b with
        | [] -> e
        | (pos, inst)::remaining -> eval_instruction inst |> eval_block remaining

    in
    let env = Environment.empty in
    eval_block p env
;;

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
