open AbstractSyntax;;

(** Applique l'opérateur operator sur les deux nombres a et b*)
let applyOperator (operator:op) (a:int) (b:int) =
  match operator with
  | Add -> a + b
  | Sub -> a - b
  | Mul -> a * b
  | Div -> a / b
  | Mod -> a mod b
;;

(** Applique le comparateur c aux deux nombres a et b*)
let applyComparator (c:comp) (a:int) (b:int) =
  match c with
  | Eq -> a = b
  | Ne -> a <> b
  | Lt -> a < b
  | Le -> a <= b
  | Gt -> a > b
  | Ge -> a >= b
;;

(** Simplifie une expression e "évidente", avec les règles de simplification de base, comme
0 + a = a, a - 0 = a, a * 0 = 0, a * 1 = a, 0 / a = 0 et a / 1 = a
*)
let simplify_obvious_operation (operator:op) (e1:expr) (e2:expr) =
  match operator with
  | Add ->
    if e1 = Num 0
    then Some e2
    else
      if e2 = Num 0
      then Some e1
      else None
  | Sub ->
    if e2 = Num 0
    then Some e1
    else None
  | Mul -> (
    match e1 with
    | Num 0 -> Some (Num 0)
    | Num 1 -> Some e2
    | _ ->
      match e2 with
      | Num 0 -> Some (Num 0)
      | Num 1 -> Some e1
      | _ -> None
  )
  | Div -> (
    match e1 with
    | Num 0 -> Some (Num 0)
    | _ ->
      match e2 with
      | Num 1 -> Some e1
      | _ -> None
  )
  | Mod -> None
;;

(** Simplifie récursivement une expression en appliquant la simplification aux deux côtés d'une
opération*)
let rec simplify_expr (ex : expr) =
  match ex with
  | Op(operator, e1, e2) -> (
    match (simplify_obvious_operation operator e1 e2) with
    | Some e -> e
    | None ->
      (* Si il n'y a pas de simplification évidente, alors on simplifie récursivement*)
      let simplifiedEx1 = simplify_expr e1 in
      let simplifiedEx2 = simplify_expr e2 in
      match simplifiedEx1, simplifiedEx2 with 
      (* Si les deux expression simplifiées sont ramenées a des entiers, alors appliquer l'opérateur
      sur ces entiers*)
      | Num a, Num b -> (
        try
          let result = applyOperator operator a b in
          Num result
        with Failure msg -> Op(operator, Num a, Num b)
        (* ^ Si il y a un problème lors de l'application, on reprend l'expression de base*)
      )
      | _, _ -> Op(operator, simplifiedEx1, simplifiedEx2)
  )
  | x -> x
;;

(** Simplifie un sous bloc d'un if ou d'un while si la condition est évidemment valide ou fausse*)
let rec simplify_if_or_while (isIf:bool) (c:cond) (bt:block) (bf:block) (pos:int) =
  let condE1, comparator, condE2 = c in
  let sCondE1 = simplify_expr condE1 in
  let sCondE2 = simplify_expr condE2 in
  match sCondE1, sCondE2 with
  | Num a, Num b ->
    if (applyComparator comparator a b)
    then
      if isIf
      then simplify_polish bt
      else [pos, While(c, simplify_polish bt)]
    else
      if isIf
      then simplify_polish bf
      else []
  | _, _ ->
    if isIf
    then [pos, If(c, simplify_polish bt, simplify_polish bf)] 
    else [pos, While(c, simplify_polish bt)]

(** Simplifie un bloc de code polish*)
and simplify_polish (p : program) =
  let rec aux prog finalprog =
    if prog = []
    then finalprog
    else
      let pos, inst = List.hd prog in
      let rest = List.tl prog in
      let newInst = match inst with
      | Set(n, e) -> [pos, Set(n, simplify_expr e)]
      | Print(e) -> [pos, Print(simplify_expr e)]
      | If(c, bt, bf) -> (simplify_if_or_while true c bt bf pos)
      | While(c, b) -> (simplify_if_or_while false c b [] pos)
      | Read(name) -> [pos, Read(name)]
      in aux rest (finalprog @ newInst)
  in
  aux p []
;;