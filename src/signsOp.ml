open Set;; 
open Printf;;

exception NoSignMatch;; 

type sign = Pos | Neg | Zero | Err

module Sign =
struct
  type t = sign
  let compare x y =
    if (x = y)
    then 0
    else 1
end 

module SignSet = Set.Make(Sign);;

let s_of_l li =
  List.fold_left (fun s e -> SignSet.add e s) SignSet.empty li

let pos = [Pos] |> s_of_l;;
let neg = [Neg] |> s_of_l;;
let zero = [Zero] |> s_of_l;;
let poszero = [Pos; Zero] |> s_of_l;;
let negzero = [Neg; Zero] |> s_of_l;;
let error = [Err] |> s_of_l;; 
let posneg = [Pos; Neg] |> s_of_l;;
let all = [Pos; Neg; Zero] |> s_of_l;;

let rec get_comb li s1 s2 commut = 
  let s1 = SignSet.remove Err s1 in
  let s2 = SignSet.remove Err s2 in
  let result = List.assoc_opt (s1, s2) li in
  match result with
  | Some s -> s
  | None ->
      if commut
      then get_comb li s2 s1 false
      else raise NoSignMatch
;; 

let print_sign_set s =
  let signToString si =
    match si with
    | Pos -> "+"
    | Neg -> "-"
    | Zero -> "0"
    | Err -> "!"
  in
  SignSet.iter (fun e -> printf "%s" (signToString e)) s;
  print_string "\n"
;;
  
let add_signs_tab = 
  [ 
    ((pos, pos), pos);
    ((neg, neg), neg); 
    ((pos, neg), all); 
    ((poszero, pos), pos);
    ((negzero, pos), all);
    ((poszero, neg), all);
    ((negzero, neg), neg); 
  ] ;; 

let get_add_sign firstSet secondSet = 
  if firstSet = zero
  then secondSet
  else if secondSet = zero
  then firstSet
  else if firstSet = posneg || firstSet = all
          || secondSet = posneg || secondSet = all
  then all
  else
    get_comb add_signs_tab firstSet secondSet true 
;;

let mul_signs_tab = 
  [ 
    ((pos, pos), pos);
    ((neg, neg), pos); 
    ((pos, neg), neg); 
    ((posneg, pos), posneg);
    ((posneg, neg), posneg);
    ((posneg, posneg), posneg);
  ] ;; 

let get_mul_sign firstSet secondSet =
  if firstSet = zero || secondSet = zero
  then zero
  else
    let comb = get_comb mul_signs_tab
        (SignSet.remove Zero firstSet)
        (SignSet.remove Zero secondSet)
        true
    in
    if SignSet.mem Zero firstSet || SignSet.mem Zero secondSet
    then SignSet.add Zero comb
    else comb
;;

let sub_signs_tab =
  [
    ((zero, pos), neg);
    ((zero, neg), pos);
    ((zero, zero), zero);
    ((zero, poszero), negzero);
    ((zero, negzero), poszero);
    ((zero, posneg), posneg);
    ((zero, all), all);
    ((pos, pos), all);
    ((neg, neg), all);
    ((pos, neg), pos);
    ((neg, pos), neg);
    ((poszero, pos), all);
    ((pos, poszero), all);
    ((poszero, neg), pos);
    ((neg, poszero), neg);
    ((negzero, pos), neg);
    ((pos, negzero), pos);
    ((negzero, neg), all);
    ((neg, negzero), all);
    ((poszero, negzero), all);
    ((negzero, poszero), all);
  ]

let get_sub_sign firstSet secondSet =
  if secondSet = zero
  then firstSet
  else
    let rmZE = (fun s -> s |> SignSet.remove Zero |> SignSet.remove Err) in
    let fS = rmZE firstSet in
    let sS = rmZE secondSet in
    if fS = posneg || (firstSet <> zero && (sS = posneg))
    then all
    else get_comb sub_signs_tab firstSet secondSet false
;;

let div_signs_table = mul_signs_tab;;

let get_div_sign firstSet secondSet =
  if secondSet = zero
  then error
  else
    let res =
      if firstSet = zero
      then zero
      else
        let comb = get_comb mul_signs_tab
            (SignSet.remove Zero firstSet)
            (SignSet.remove Zero secondSet)
            true
        in
        if SignSet.mem Zero firstSet
        then SignSet.union zero comb
        else comb
    in
    if SignSet.mem Zero secondSet
    then SignSet.union res error
    else res
;;