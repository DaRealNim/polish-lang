open AbstractSyntax;;
open Set;;

module Names = Set.Make(String)


(* Renvoie une liste d'options où (v : name) -> Some(v) et (n : num) -> None *)
let rec vars_expr_opt e =
	match e with
		| Var(name) -> [Some(name)]
		| Num(_) -> [None]
		| Op(op,expr1,expr2) -> vars_expr_opt expr1 @ vars_expr_opt expr2

(* Renvoie une liste d'options sans ses éléments None *)
let clean_none opt_list =
	List.filter (fun x -> x != None) opt_list

(* "Déballe" une liste d'options, évoque Invalid_argument s'il essaye de déballer un élément None *)
let unwrap opt_list =
	List.map (fun x -> Option.get x) opt_list

(* Renvoie une liste avec les éléments de list absents de set*)
let absent_elts set list =
	List.filter (fun x -> not (Names.mem x set)) list

(*Rajoute les éléments de la liste namesOpt absents dans names dans l'ensemble badNames et le renvoie*)
let rec new_bad_names names badNames namesOpt =
	clean_none namesOpt |> unwrap |> absent_elts names |>
	List.fold_left (fun badNames x -> Names.add x badNames) badNames

let rec vars_polish (p : program) names badNames =
	let pos, instr = List.hd p in
	let rest = List.tl p in
	match instr with
		| If(c,bt ,bf) -> Names.empty
		| While(c, b) -> Names.empty
		| Read(name) -> 
			if Names.mem name names then
				vars_polish rest names badNames
			else
				vars_polish rest names (Names.add name badNames)
		| Set(name, _) -> vars_polish rest (Names.add name names) badNames
		| Print(e) -> vars_polish rest names (new_bad_names names badNames (vars_expr_opt e))
;;