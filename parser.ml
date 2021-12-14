(** Projet Polish -- Analyse statique d'un mini-langage impératif *)
(* Parser est le module permettant la lecture du fichier .p et sa conversion
en syntaxe abstraite ocaml, qui pourra ensuite être ré-affichée par Printer
ou executée par VirtualMachine. *)

open AbstractSyntax;;

exception WrongIndentation;;
exception ExpectedIndentedBlock;;
exception ElseWithoutIf;;
exception WrongCondition;;
exception WrongExpression;;

let try_read channel =
  try Some (input_line channel) with End_of_file -> None
;;

let rec read_loop acc i channel =
  match try_read channel with
  | Some s -> read_loop ((i, s)::acc) (i + 1) channel
  | None -> close_in channel; List.rev acc
;;

let rec count_spaces l count =
  match l with
  | ""::xs -> count_spaces xs (count + 1)
  | _ -> count
;;

let rec cut_n_elements l n =
  if n > 0
  then cut_n_elements (List.tl l) (n-1)
  else l
;;


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
;;


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
;;


let read_expr e =
  let expr = make_expr e in
  match expr with
  | (exp, reste) ->
      if List.length reste <> 0 then
        raise WrongExpression
      else
        exp
;;


let rec read_block lines depth acc =
  match lines with
  | [] -> List.rev acc, []
  | (pos, x)::xs ->
      let line = (String.split_on_char ' ' x) in
      let spaces = (count_spaces line 0) in
      let indent = spaces / 2 in
      if     (List.length line) = spaces
          || (List.hd (cut_n_elements line (indent*2))) = "COMMENT"
      then read_block xs depth acc
      else (
        if (indent >= depth) then
          let inst, suite = read_instruction line depth xs in
          match suite with
          | Some suite -> read_block suite depth ((pos, inst)::acc)
          | None -> read_block xs depth ((pos, inst)::acc)
        else
          List.rev acc, lines
      )

and read_instruction line depth rest =
  let ind = (count_spaces line 0) in
  if (ind mod 2 <> 0) || (ind / 2 <> depth) then
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
        if (List.length whileblock) = 0
        then raise ExpectedIndentedBlock
        else While(read_condition c, whileblock), Some suite
    | "IF"::c -> (
        let ifblock, suite = read_block rest (ind + 1) [] in
        if (List.length ifblock) = 0
        then raise ExpectedIndentedBlock
        else ();
        match suite with
        | (pos, inst)::xs ->
            let elseblock, suite =
              if (String.trim inst) = "ELSE"
              then
                let block, suite = read_block (cut_n_elements suite 1) (ind + 1) [] in
                if (List.length block) = 0
                then raise ExpectedIndentedBlock
                else block, suite
              else
                [], suite
            in
            If(read_condition c, ifblock, elseblock), Some suite
        | [] -> If(read_condition c, ifblock, []), None
      )
    | "ELSE"::reste -> raise ElseWithoutIf
    | _ -> failwith "unknown error"
;;


let read_polish (filename:string) : program =
  let in_channel = open_in filename in
  let main_line_list = read_loop [] 0 in_channel in
  let output, _ = read_block main_line_list 0 []
  in output
;;
