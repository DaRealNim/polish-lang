exception NoSignMatch
module Sign :
  sig type t = Pos | Neg | Zero | Err val compare : t -> t -> int end

val pos : Set.Make(Sign).t
val neg : Set.Make(Sign).t
val zero : Set.Make(Sign).t
val poszero : Set.Make(Sign).t
val negzero : Set.Make(Sign).t
val error : Set.Make(Sign).t
val posneg : Set.Make(Sign).t
val all : Set.Make(Sign).t

val get_add_sign : Set.Make(Sign).t -> Set.Make(Sign).t -> Set.Make(Sign).t
val get_mul_sign : Set.Make(Sign).t -> Set.Make(Sign).t -> Set.Make(Sign).t
val get_sub_sign : Set.Make(Sign).t -> Set.Make(Sign).t -> Set.Make(Sign).t
val get_div_sign : Set.Make(Sign).t -> Set.Make(Sign).t -> Set.Make(Sign).t
val get_mod_sign : Set.Make(Sign).t -> Set.Make(Sign).t -> Set.Make(Sign).t