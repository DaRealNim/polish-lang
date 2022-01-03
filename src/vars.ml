open AbstractSyntax;;
open Set;;
open Printf;;

module Names = Set.Make(String)
let rec vars_expr_opt e =
	match e with
		| Var(name) -> [name]
		| Num(_) -> []
		| Op(op,expr1,expr2) -> vars_expr_opt expr1 @ vars_expr_opt expr2

let vars_cond_opt c =
	let (e1, _, e2) = c in
	vars_expr_opt e1 @ vars_expr_opt e2

(* Renvoie une liste avec les éléments de list absents de set*)
let absent_elts set list =
	List.filter (fun x -> not (Names.mem x set)) list


(*Rajoute les éléments de la liste namesOpt absents dans names dans l'ensemble badNames et le renvoie*)
let new_bad_names names badNames nameList =
	List.fold_left (fun badNames x -> Names.add x badNames) badNames (absent_elts names nameList)

let add_good_subs names subNames badNames =
	Names.union names (Names.diff subNames badNames)

let rec if_and_while (p : program) names subNames badNames bt bf =
	let names1, subNames1, badNames1 = vars_block bt names subNames badNames in
	let names2, subNames2, badNames2 = vars_block bf names subNames badNames in
	let namesUnion = (Names.union names1 names2) in
	let subNamesUnion = (Names.union subNames1 subNames2) in
	let badNamesUnion = (Names.union badNames1 badNames2) in
	(namesUnion , (Names.inter subNames1 subNames2), Names.union badNames1 badNames2)

and vars_block (p : program) names subNames badNames =
	if p = [] then
		(names, subNames, badNames)
	else
		let pos, instr = List.hd p in
		let rest = List.tl p in
		match instr with
			| If(c, bt ,bf) -> 
				let cBadNames = new_bad_names names badNames (vars_cond_opt c) in
				let setNames, setSubNames, setBadNames = (if_and_while p names subNames cBadNames bt bf) in
				vars_block rest setNames setSubNames setBadNames
			| While(c, b) -> 
				let cBadNames = new_bad_names names badNames (vars_cond_opt c) in
				let setNames, setSubNames, setBadNames = (if_and_while p names subNames cBadNames b []) in
				vars_block rest setNames setSubNames setBadNames
			| Read(name) -> vars_block rest (Names.add name names) (Names.add name subNames) badNames
			| Set(name, e) -> let newNames = (Names.add name names) in
			vars_block rest newNames (Names.add name subNames) (new_bad_names subNames badNames (vars_expr_opt e))
			| Print(e) -> vars_block rest names subNames (new_bad_names subNames badNames (vars_expr_opt e))
;;

let vars_polish (p : program) =
	let (names, subNames, badNames) = vars_block p Names.empty Names.empty Names.empty in
	let names2 = add_good_subs names subNames badNames in
	Seq.iter (fun x -> printf "%s " x) (Names.to_seq (Names.union names2 badNames));
	printf("\n");
	Seq.iter (fun x -> printf "%s " x) (Names.to_seq badNames)