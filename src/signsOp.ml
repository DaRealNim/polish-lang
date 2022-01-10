open Set;; 
open Printf;;

exception NoSignMatch;; 

module Sign =
struct
  type t = Pos | Neg | Zero | Err
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

let s_of_l li =
  List.fold_left (fun s e -> SignSet.add e s) SignSet.empty li

let pos = [Sign.Pos] |> s_of_l;;
let neg = [Sign.Neg] |> s_of_l;;
let zero = [Sign.Zero] |> s_of_l;;
let poszero = [Sign.Pos; Sign.Zero] |> s_of_l;;
let negzero = [Sign.Neg; Sign.Zero] |> s_of_l;;
let error = [Sign.Err] |> s_of_l;; 
let posneg = [Sign.Pos; Sign.Neg] |> s_of_l;;
let all = [Sign.Pos; Sign.Neg; Sign.Zero] |> s_of_l;;

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
  print_string "add\n";
  if SignSet.equal firstSet zero
  then secondSet
  else if SignSet.equal secondSet zero
  then firstSet
  else if SignSet.equal firstSet posneg || SignSet.equal firstSet all
          || SignSet.equal secondSet posneg || SignSet.equal secondSet all
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
  print_string "mul\n";
  if SignSet.equal firstSet zero || SignSet.equal secondSet zero
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
  print_string "sub\n";
  if SignSet.equal secondSet zero
  then firstSet
  else
    let rmZE = (fun s -> s |> SignSet.remove Zero |> SignSet.remove Err) in
    let fS = rmZE firstSet in
    let sS = rmZE secondSet in
    if SignSet.equal fS posneg || (not (SignSet.equal firstSet zero) && (SignSet.equal sS posneg))
    then all
    else get_comb sub_signs_tab firstSet secondSet false
;;

let div_signs_table = mul_signs_tab;;

let mod_signs_table = 
  [
    ((pos, pos), poszero);
    ((neg, neg), negzero);
    ((neg, pos), negzero);
    ((pos, neg), poszero);
    ((posneg, pos), all);
    ((pos, posneg), poszero);
    ((posneg, neg), all);
    ((neg, posneg), negzero);
    ((posneg, posneg), all);
    ((all, all), all);
    ((posneg, all), posneg);
  ];;

let get_div_mod_sign firstSet secondSet isMod =
  print_string "div/mod\n";
  if SignSet.equal secondSet zero
  then error
  else
    let res =
      if SignSet.equal firstSet zero
      then zero
      else
        let tab = if isMod
                  then mod_signs_table
                  else div_signs_table
        in
        let comb = get_comb tab
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