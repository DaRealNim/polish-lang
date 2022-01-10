(** Ce module rassemble les tableau d'associations ainsi que les fonctions
pour déterminer le résultat d'une opération sur deux ensembles de signes*)

open Set;; 
open Printf;;

exception NoSignMatch;; 

(** Un type custom Sign. Une fonction compare y est défini afin d'en faire un
OrderedType, et qu'il soit accepté par Set, pour faire un SignSet*)
module Sign =
struct
  type t = Pos | Neg | Zero | Err
  
  (** Pour la fonction compare. Il se trouve que simplement comparer deux signes,
  e.g Pos = Neg, ne marche pas toujours et posait des problèmes lors des opération sur
  les ensembles (union, intersections...), on pouvait trouver des doublons.*)
  let comp_to_int comp =
    match comp with
    | Pos -> 0
    | Neg -> 1
    | Zero -> 2
    | Err -> 3
  ;;
  let compare x y =
    let a, b = comp_to_int x, comp_to_int y in
    if a = b
    then 0
    else if a < b
    then -1
    else 1
end 

module SignSet = Set.Make(Sign);;

(** Fonction de débug pour afficher un ensemble de signes*)
let print_sign_set s =
  let signToString si =
    match si with
    | Sign.Pos -> "+"
    | Sign.Neg -> "-"
    | Sign.Zero -> "0"
    | Sign.Err -> "!"
  in
  SignSet.iter (fun e -> printf "%s" (signToString e)) s;
  print_string "\n"
;;

(** Permet la conversions d'une liste de signes en un set de list*)
let s_of_l li =
  List.fold_left (fun s e -> SignSet.add e s) SignSet.empty li

(* Ici, quelques constantes de sets pour faciliter la lisibilité du code plus bas *)

(** Le set {+} *)
let pos = [Sign.Pos] |> s_of_l;;

(** Le set {-} *)
let neg = [Sign.Neg] |> s_of_l;;

(** Le set {0} *)
let zero = [Sign.Zero] |> s_of_l;;

(** Le set {+, 0} *)
let poszero = [Sign.Pos; Sign.Zero] |> s_of_l;;

(** Le set {-, 0} *)
let negzero = [Sign.Neg; Sign.Zero] |> s_of_l;;

(** Le set {!} *)
let error = [Sign.Err] |> s_of_l;; 

(** Le set {+, -} *)
let posneg = [Sign.Pos; Sign.Neg] |> s_of_l;;

(** Le set {+, -, 0} *)
let all = [Sign.Pos; Sign.Neg; Sign.Zero] |> s_of_l;;

(** Fais l'union des résultats d'une opération sur les signes a et b,
ou a et b sont toutes les combinaisons des ensembles de signes s1 et s2.
Le résultat est donné par la liste d'association li *)
let compute_signs li s1 s2 =
  let s1 = SignSet.remove Err s1 in
  let s2 = SignSet.remove Err s2 in
  let res = SignSet.fold
    (fun sign1 acc ->
      SignSet.fold
      (fun sign2 acc2 ->
        let res = List.assoc (sign1, sign2) li in
        SignSet.union acc2 res
      ) s2 acc
    )
    s1 SignSet.empty
  in res
;;

(** Liste d'association de signes pour l'addition *)
let add_signs_assoclist =
  [
    ((Sign.Pos, Sign.Neg), all);
    ((Sign.Pos, Sign.Pos), pos);
    ((Sign.Neg, Sign.Pos), all);
    ((Sign.Neg, Sign.Neg), neg);
    ((Sign.Zero, Sign.Pos), pos);
    ((Sign.Pos, Sign.Zero), pos);
    ((Sign.Zero, Sign.Neg), neg);
    ((Sign.Neg, Sign.Zero), neg);
    ((Sign.Zero, Sign.Zero), zero);
  ];;

let get_add_sign firstSet secondSet = 
  compute_signs add_signs_assoclist firstSet secondSet
;;

(** Liste d'association de signes pour la multiplication *)
let mul_signs_assoclist =
  [
    ((Sign.Pos, Sign.Neg), neg);
    ((Sign.Pos, Sign.Pos), pos);
    ((Sign.Neg, Sign.Pos), neg);
    ((Sign.Neg, Sign.Neg), pos);
    ((Sign.Zero, Sign.Pos), zero);
    ((Sign.Pos, Sign.Zero), zero);
    ((Sign.Zero, Sign.Neg), zero);
    ((Sign.Neg, Sign.Zero), zero);
    ((Sign.Zero, Sign.Zero), zero);
  ];;

let get_mul_sign firstSet secondSet =
  compute_signs mul_signs_assoclist firstSet secondSet
;;

(** Liste d'association de signes pour la soustraction *)
let sub_signs_assoclist =
  [
    ((Sign.Pos, Sign.Neg), pos);
    ((Sign.Pos, Sign.Pos), all);
    ((Sign.Neg, Sign.Pos), neg);
    ((Sign.Neg, Sign.Neg), all);
    ((Sign.Zero, Sign.Pos), neg);
    ((Sign.Pos, Sign.Zero), pos);
    ((Sign.Zero, Sign.Neg), pos);
    ((Sign.Neg, Sign.Zero), neg);
    ((Sign.Zero, Sign.Zero), zero);
  ];;

let get_sub_sign firstSet secondSet =
  compute_signs sub_signs_assoclist firstSet secondSet
;;

(** Liste d'association de signes pour la division *)
let div_signs_assoclist =
  [
    ((Sign.Pos, Sign.Neg), neg);
    ((Sign.Pos, Sign.Pos), pos);
    ((Sign.Neg, Sign.Pos), neg);
    ((Sign.Neg, Sign.Neg), pos);
    ((Sign.Zero, Sign.Pos), zero);
    ((Sign.Pos, Sign.Zero), error);
    ((Sign.Zero, Sign.Neg), zero);
    ((Sign.Neg, Sign.Zero), error);
    ((Sign.Zero, Sign.Zero), error);
  ];;

(** Liste d'association de signes pour le modulo *)
let mod_signs_assoclist =
  [
    ((Sign.Pos, Sign.Neg), pos);
    ((Sign.Pos, Sign.Pos), pos);
    ((Sign.Neg, Sign.Pos), neg);
    ((Sign.Neg, Sign.Neg), neg);
    ((Sign.Zero, Sign.Pos), zero);
    ((Sign.Pos, Sign.Zero), error);
    ((Sign.Zero, Sign.Neg), zero);
    ((Sign.Neg, Sign.Zero), error);
    ((Sign.Zero, Sign.Zero), error);
  ];;

let get_div_sign firstSet secondSet =
  compute_signs div_signs_assoclist firstSet secondSet
;;

let get_mod_sign firstSet secondSet =
  compute_signs mod_signs_assoclist firstSet secondSet
;;