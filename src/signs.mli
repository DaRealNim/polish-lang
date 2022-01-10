(* val sign_of : int -> SignsOp.sign
val signs_op : AbstractSyntax.op -> SignSet.t -> SignSet.t -> SignSet.t
val signs_expr : AbstractSyntax.expr -> SignSet.t SignMap.t -> SignSet.t
val spc_aux_eq : SignSet.t -> SignSet.t -> bool
val spc_aux_neq : SignSet.t -> SignSet.t -> bool
val spc_aux_lt : SignSet.t -> SignSet.t -> bool
val spc_lt_tab :
  ((SignsOp.sign * SignsOp.sign) * (SignSet.t * SignSet.t)) list
val get_assoc :
  ((SignsOp.sign * SignsOp.sign) * (SignSet.t * SignSet.t)) list ->
  SignSet.t -> SignSet.t -> SignSet.t * SignSet.t
val spc_aux_bool : SignSet.t -> AbstractSyntax.comp -> SignSet.t -> bool
val spc_aux_signsets :
  SignSet.t -> AbstractSyntax.comp -> SignSet.t -> SignSet.t * SignSet.t
val signs_propagate_condition :
  AbstractSyntax.expr * AbstractSyntax.comp * AbstractSyntax.expr ->
  signenvironment -> bool * bool * signenvironment * signenvironment
val sign_polish :
  AbstractSyntax.block -> SignSet.t SignMap.t -> SignSet.t SignMap.t *)

open AbstractSyntax;;

val test : unit