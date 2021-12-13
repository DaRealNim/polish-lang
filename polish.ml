
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
    | Some s -> read_loop ((i, s)::acc) (i + 1)
    | None -> close_in in_channel; List.rev acc
  in

  let main_line_list = read_loop [] 0
  in

  let rec count_spaces l count =
    match l with
    | ""::xs -> count_spaces xs (count + 1)
    | _ -> count
  in

  let rec cut_n_elements l n =
    if n > 0
    then cut_n_elements (List.tl l) (n-1)
    else l
  in

	let rec make_expr e =
	  match e with
	  | [] -> raise WrongExpression
	  | "+"::xs ->
	      let ex1, suite = make_expr xs in
	      let ex2, fin = make_expr suite in
	      Op(Add, ex1, ex2), fin
	  | "-"::xs ->
	      let ex1, suite = make_expr xs in
	      let ex2, fin = make_expr suite in
	      Op(Sub, ex1, ex2), fin
	  | "*"::xs ->
	      let ex1, suite = make_expr xs in
	      let ex2, fin = make_expr suite in
	      Op(Mul, ex1, ex2), fin
	  | "/"::xs ->
	      let ex1, suite = make_expr xs in
	      let ex2, fin = make_expr suite in
	      Op(Div, ex1, ex2), fin
	  | "%"::xs ->
	      let ex1, suite = make_expr xs in
	      let ex2, fin = make_expr suite in
	      Op(Mod, ex1, ex2), fin
	  | x::xs -> (
	      try
	        Num(int_of_string x), xs
	      with Failure msg -> Var(x), xs
	    )
	in
	let read_condition c =
	  let ex1, reste = make_expr c in
	  let comparator, reste = match reste with
	    | "="::xs -> Eq, xs
	    | "<>"::xs -> Ne, xs
	    | "<"::xs -> Lt, xs
	    | ">"::xs -> Gt, xs
	    | "<="::xs -> Le, xs
	    | ">="::xs -> Ge, xs
	    | _ -> raise (WrongCondition)
	  in
	  let ex2, reste = make_expr reste in
	  if reste = []
	  then (ex1, comparator, ex2)
	  else raise (WrongCondition)
	in
	let read_expr e =
	  let expr = make_expr e in
	  match expr with
	  | (exp, reste) ->
	      if List.length reste <> 0 then
	        raise WrongExpression
	      else
	        exp
  in


    let rec read_block lines depth acc =
      match lines with
      | [] -> List.rev acc, []
      | (pos, x)::xs ->
          let line = (String.split_on_char ' ' x) in
          let indent = (count_spaces line 0) / 2 in
          if (List.hd (cut_n_elements line (indent*2))) = "COMMENT"
          then read_block xs depth acc
          else (
              if (indent >= depth) then
               let inst, suite = read_instruction line indent xs in
               match suite with
               | Some suite -> read_block suite indent ((pos, inst)::acc)
               | None -> read_block xs indent ((pos, inst)::acc)
             else
               List.rev acc, lines
         )

    and read_instruction line depth rest =
      let ind = (count_spaces line 0) in
      (*let () = Printf.printf "nb spaces : %d\n" ind in*)
      if (ind mod 2 <> 0) then
        raise WrongIndentation
      else
        let ind = ind/2 in
        let line_no_ident = cut_n_elements line (ind*2) in
        match line_no_ident with
        | [] -> failwith "foo"
        | "READ"::name::reste -> Read(name), None
        | "READ"::[] -> failwith "invalid read"
        | var::":="::e -> Set(var, read_expr e), None
        | "PRINT"::e -> Print(read_expr e), None
        | "WHILE"::c ->
            let whileblock, suite = read_block rest (ind + 1) [] in
            While(read_condition c, whileblock), Some suite
        | "IF"::c -> (
            let ifblock, suite = read_block rest (ind + 1) [] in
            match suite with
            | (pos, inst)::xs ->
                let elseblock, suite =
                  if (String.trim inst) = "ELSE"
                  then
                    read_block (cut_n_elements suite 1) (ind + 1) []
                  else
                    [], suite
                in
                If(read_condition c, ifblock, elseblock), Some suite
            | [] -> If(read_condition c, ifblock, []), None
          )
        | "ELSE"::reste -> raise ElseWithoutIf
        | _ -> failwith "unknown error"
  in
  let output, _ = read_block main_line_list 0 []
  in output
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
  in
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


  and print_block (p:program) (level:int) : unit =
    match p with
    | [] -> ();
    | (pos, instruction)::rest ->
        (* Printf.printf "%d : " pos; *)
        let newline = print_instruction instruction level in
        (* Printf.printf "(%d / %d)" (List.length rest) level; *)
        if newline
        then Printf.printf "\n"
        else ();
        print_block rest level;
  in
  print_block p 0
;;

module Environment = Map.Make(String);;
let eval_polish (p:program) : unit =
    let get_variable (var:name) (e:int Environment.t) =
        let v = Environment.find_opt var e in
        match v with
        | Some(value) -> value
        | None -> failwith (Printf.sprintf "Error: accessed variable %s before setting it" var)
    in
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
    in
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
    in
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
            (*ERROR : these must be mutually recursive (TODO)*)
            if (eval_condition cond e)
            then eval_block blockTrue e
            else eval_block blockFalse e
        | While (cond, blockRepeat) ->
            (*ERROR: same (TODO)*)
            if (eval_condition cond e)
            then (eval_block blockRepeat e) |> eval_instruction i
            else e

    and eval_block (b:block) (e:int Environment.t) =
        match b with
        | [] -> e
        | (pos, inst)::remaining -> (eval_instruction inst e) |> eval_block remaining

    in
    let env = Environment.empty in
    let _ = eval_block p env in
    ()
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
