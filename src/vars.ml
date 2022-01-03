open AbstractSyntax;;
open Set;;
open Printf;;

module Vars = Set.Make(String)

(* Parse une expression et renvoie une liste de ses variables *)
let rec vars_expr e =
	match e with
		| Var(name) -> [name]
		| Num(_) -> []
		| Op(op,expr1,expr2) -> vars_expr expr1 @ vars_expr expr2

(* Parse une condition et renvoie une liste de ses variables *)
let vars_cond c =
	let (e1, _, e2) = c in
	vars_expr e1 @ vars_expr e2

(* Renvoie une liste avec les éléments de list absents de set *)
let absent_elts set list =
	List.filter (fun x -> not (Vars.mem x set)) list

(* Rajoute les éléments de la liste varList non inclus dans vars 
 * dans l'ensemble badVars et renvoie badVars *)
let add_bad_vars vars badVars varList =
	List.fold_left (fun badVars x -> Vars.add x badVars) badVars (absent_elts vars varList)

(* Parse le bloc if d'un programme pour chercher ses variables principales,  
 * subvariables et variables dangereuses *)
let rec if_block_vars (p : program) vars subBlockVars badVars tb fb =
	let varsTB, subVarsTB, badVarsTB = vars_block tb vars subBlockVars badVars in
	let varsFB, subVarsFB, badVarsFB = vars_block fb vars subBlockVars badVars in

	let varsUnion = (Vars.union varsTB varsFB) in
	let subVarsInter = (Vars.inter subVarsTB subVarsFB) in
	let badVarsUnion = (Vars.union badVarsTB badVarsFB) in

	(varsUnion, subVarsInter, badVarsUnion)
(* Parse un programme et renvoie un tuple de la forme (vars, subBlockVars, badVars) 
 * où les noms dans vars sont dans le bloc principal,
 * celles de subBlockVars dans des sous-blocs et du bloc principal,
 * et badVars celles qui risquent de provoquer une erreur d'accès *)	
and vars_block (p : program) vars subBlockVars badVars =
	if p = [] then
		(vars, subBlockVars, badVars)
	else
		let _, instr = List.hd p in
		let rest = List.tl p in
		match instr with
			| If(cond, tb, fb) -> 
				let condBadVars = add_bad_vars vars badVars (vars_cond cond) in
				let blockVars, blockSubVars, blockBadVars = (if_block_vars p vars subBlockVars condBadVars tb fb) in
				vars_block rest blockVars blockSubVars blockBadVars
			| While(cond, b) -> 
				let condBadVars = add_bad_vars vars badVars (vars_cond cond) in
				vars_block b vars subBlockVars condBadVars
			| Read(name) -> vars_block rest (Vars.add name vars) (Vars.add name subBlockVars) badVars
			| Set(name, e) -> 
				let newVars = (Vars.add name vars) in
				let newSubVars = (Vars.add name subBlockVars) in
				let newBadVars = add_bad_vars subBlockVars badVars (vars_expr e) in
				vars_block rest newVars newSubVars newBadVars
			| Print(e) -> vars_block rest vars subBlockVars (add_bad_vars subBlockVars badVars (vars_expr e))
;;

(* Affiche toutes les variables d'un programme dans une ligne, et celles
 * potentiellement dangereuses dans une deuxième *)
let vars_polish (p : program) =
	let (vars, subBlockVars, badVars) = vars_block p Vars.empty Vars.empty Vars.empty in
	let allVars = Vars.union (Vars.union vars subBlockVars) badVars in
	Seq.iter (fun x -> printf "%s " x) (Vars.to_seq allVars);
	printf("\n");
	Seq.iter (fun x -> printf "%s " x) (Vars.to_seq badVars)