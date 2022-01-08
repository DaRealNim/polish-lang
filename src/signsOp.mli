exception NoSignMatch
type sign = Pos | Neg | Zero | Err
module Sign : sig type t = sign val compare : 'a -> 'a -> int end

val print_sign_set : Set.Make(Sign).t -> unit

val all : Set.Make(Sign).t
  
val get_add_sign : Set.Make(Sign).t -> Set.Make(Sign).t -> Set.Make(Sign).t

val get_mul_sign : Set.Make(Sign).t -> Set.Make(Sign).t -> Set.Make(Sign).t

val get_sub_sign : Set.Make(Sign).t -> Set.Make(Sign).t -> Set.Make(Sign).t

val get_div_mod_sign : Set.Make(Sign).t -> Set.Make(Sign).t -> bool -> Set.Make(Sign).t
